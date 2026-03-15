{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Miso
import Miso.String (MisoString, ms)

import Data.List (dropWhileEnd, intercalate, isInfixOf)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

import Data.Char (isDigit, isUpper, toLower)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Javascript.JSaddle (eval, valToStr)
import qualified Data.JSString as JS
import Language.Javascript.JSaddle.Warp (run)

-- ── Storage key ────────────────────────────────────────────────────────────

storageKey :: String
storageKey = "cert-quiz-answers"

-- ── Domain types ───────────────────────────────────────────────────────────

data Question = Question
  { questionId      :: Int
  , questionText    :: MisoString
  , questionCode    :: MisoString       -- "" when there is no code block
  , questionOptions :: [(Char, MisoString)]
  , isMultiple      :: Bool             -- True when question says "Choose N"
  }
  deriving (Show, Eq)

data Model = Model
  { questionsList   :: [Question]
  , currentQuestion :: Int              -- == length questionsList means "last page"
  , selectedAnswers :: M.Map Int (S.Set Char)
  , correctAnswers  :: M.Map Int (S.Set Char)
  , explanations    :: M.Map Int String
  , showResults     :: Bool
  }
  deriving (Show, Eq)

data Action
  = NextQuestion
  | PrevQuestion
  | ToggleAnswer Char
  | SetQuestions [Question] (M.Map Int (S.Set Char)) (M.Map Int String)
  | LoadAnswers String
  | Evaluate
  | BackToQuiz
  | NoOp
  deriving (Show, Eq)

-- ── Parsing: questions1.txt ────────────────────────────────────────────────

startsWithLetterDot :: String -> Bool
startsWithLetterDot str =
  case span isUpper str of
    ("", _)      -> False
    (_, '.' : _) -> True
    _            -> False

startsWithNumberDot :: String -> Bool
startsWithNumberDot str =
  case span isDigit str of
    ("", _)      -> False
    (_, '.' : _) -> True
    _            -> False

splitByAString :: [String] -> [[String]]
splitByAString [] = []
splitByAString xs =
  let (_, rest) = span (\s -> null s || head s /= 'A') xs
   in case rest of
        [] -> []
        (a : xs') ->
          let (grp, remaining) = span (\s -> null s || head s /= 'A') xs'
           in (a : grp) : splitByAString remaining

parseNumberDot :: String -> (Int, String)
parseNumberDot str =
  let (digits, rest) = span isDigit str
      number = read digits :: Int
      body   = dropWhile (== ' ') (drop 1 rest)
   in (number, body)

linesWithNumbersDot :: [String] -> [(Int, String)]
linesWithNumbersDot mylines =
  filter (\x -> startsWithNumberDot $ snd x) (zip [0 ..] mylines)

getQuestions :: [(Int, (Int, String))] -> (M.Map Int String, [Int])
getQuestions m = (M.fromList $ map snd m, map fst m)

toMap :: [String] -> M.Map Char String
toMap xs = M.fromList $ mapMaybe myparse xs
 where
  myparse s = case s of
    (c : '.' : rest) | isUpper c -> Just (c, dropWhile (== ' ') rest)
    _                            -> Nothing

readAnswers :: [String] -> M.Map Int (M.Map Char String)
readAnswers mylines =
  let linesWithLetters = filter startsWithLetterDot mylines
      groups = splitByAString linesWithLetters
      m      = map toMap groups
   in M.fromList (zip [0 ..] m)

readQuestions :: [String] -> (M.Map Int String, [Int])
readQuestions ls =
  let linesWithNumbers = linesWithNumbersDot ls
      m = map (\x -> (fst x, parseNumberDot $ snd x)) linesWithNumbers
   in getQuestions m

-- | Lines between a question line and its first answer line form the code block.
readCodeBlocks :: [String] -> M.Map Int MisoString
readCodeBlocks ls = M.fromList $ go ls
 where
  go []        = []
  go (l : rest)
    | startsWithNumberDot l =
        let (qNum, _) = parseNumberDot l
            codeLines = takeWhile isCodeLine rest
            trimmed   = dropWhileEnd null codeLines
         in if null trimmed
              then go rest
              else (qNum, ms (unlines trimmed)) : go rest
    | otherwise = go rest
  isCodeLine s = not (startsWithLetterDot s) && not (startsWithNumberDot s)

-- ── Parsing: answers1.txt ──────────────────────────────────────────────────

-- | Each answer line looks like:
--     "1.  A.   The Duration ..."
--     "3.  A, C.   An identifier ..."
--     "14. A, C, E.   Options ..."
-- We read the letters that appear before the first '.' in the rest.
parseCorrectAnswers :: [String] -> M.Map Int (S.Set Char)
parseCorrectAnswers ls = M.fromList $ mapMaybe parseLine ls
 where
  parseLine s
    | startsWithNumberDot s =
        let (qNum, rest)     = parseNumberDot s
            (letterPart, _ ) = break (== '.') rest
            letters          = filter isUpper letterPart
         in if null letters
              then Nothing
              else Just (qNum, S.fromList letters)
    | otherwise = Nothing

-- | Parse the explanation text for every answer entry.
--   The explanation starts after the first ". " on the numbered line and
--   continues across all following lines until the next numbered entry.
parseExplanations :: [String] -> M.Map Int String
parseExplanations ls = M.fromList $ go ls
 where
  go [] = []
  go (l : rest)
    | startsWithNumberDot l =
        let (qNum, afterNum) = parseNumberDot l
            contLines        = takeWhile (not . startsWithNumberDot) rest
            remaining        = dropWhile (not . startsWithNumberDot) rest
            expl             = extractExpl afterNum contLines
         in (qNum, expl) : go remaining
    | otherwise = go rest

  -- afterNum = "A.   The Duration class ..."  or  "A, C, E.   Options ..."
  -- Skip past the first '.' (end of letter list) then trim spaces.
  extractExpl afterNum contLines =
    let afterDot = drop 1 (dropWhile (/= '.') afterNum)
        firstBit = dropWhile (== ' ') afterDot
        allLines = firstBit : contLines
     in unwords $ filter (not . null) $ map (dropWhile (== ' ')) allLines

-- ── Multiple-choice detection ──────────────────────────────────────────────

detectMultiple :: String -> Bool
detectMultiple s =
  let lower = map toLower s
   in any (`isInfixOf` lower)
        ["choose two", "choose three", "choose four", "choose five"]

-- ── localStorage serialisation  "1:AC|2:B|3:ABD" ──────────────────────────

serializeAnswers :: M.Map Int (S.Set Char) -> String
serializeAnswers m =
  intercalate "|" $
    map (\(k, v) -> show k <> ":" <> S.toList v) (M.toList m)

deserializeAnswers :: String -> M.Map Int (S.Set Char)
deserializeAnswers "" = M.empty
deserializeAnswers s  = M.fromList $ mapMaybe parseEntry (splitOn '|' s)
 where
  parseEntry entry = case break (== ':') entry of
    (numStr, ':' : chars) ->
      (\k -> (k, S.fromList chars)) <$> (readMaybe numStr :: Maybe Int)
    _ -> Nothing
  splitOn _ "" = [""]
  splitOn c (x : xs)
    | x == c    = "" : splitOn c xs
    | otherwise = let (hd : tl) = splitOn c xs in (x : hd) : tl

-- ── Build quiz ─────────────────────────────────────────────────────────────

buildQuiz
  :: (M.Map Int String, [Int])
  -> M.Map Int (M.Map Char String)
  -> M.Map Int MisoString
  -> [Question]
buildQuiz (qMap, _) aMap codeMap = map toQuestion (M.toAscList qMap)
 where
  toQuestion (qNum, qText) =
    let options = M.findWithDefault M.empty (qNum - 1) aMap
        code    = M.findWithDefault ""       qNum      codeMap
     in Question
          { questionId      = qNum
          , questionText    = ms qText
          , questionCode    = code
          , questionOptions = map (\(k, v) -> (k, ms v)) (M.toAscList options)
          , isMultiple      = detectMultiple qText
          }

-- ── Entry point ────────────────────────────────────────────────────────────

main :: IO ()
main = do
  content1 <- readFile "questions1.txt"
  content2 <- readFile "answers1.txt"
  let mylines   = lines content1
      questions = readQuestions mylines
      ans       = readAnswers   mylines
      codes     = readCodeBlocks mylines
      qs        = buildQuiz questions ans codes
      correct   = parseCorrectAnswers (lines content2)
      expls     = parseExplanations   (lines content2)
  run 3000 $
    startApp
      App
        { initialAction = SetQuestions qs correct expls
        , model         = Model [] 0 M.empty M.empty M.empty False
        , update        = updateModel
        , view          = viewModel
        , events        = defaultEvents
        , subs          = []
        , mountPoint    = Nothing
        , logLevel      = Off
        }

-- ── Update ─────────────────────────────────────────────────────────────────

updateModel :: Action -> Model -> Effect Action Model
updateModel = \case
  -- Allow advancing one past the last question to reach the "last page"
  NextQuestion -> \m ->
    noEff $ m{currentQuestion = min (length (questionsList m)) (currentQuestion m + 1)}

  PrevQuestion -> \m ->
    noEff $ m{currentQuestion = max 0 (currentQuestion m - 1)}

  ToggleAnswer opt -> \m ->
    case currentQuestionData m of
      Nothing -> noEff m
      Just q  ->
        let qid        = questionId q
            cur        = M.findWithDefault S.empty qid (selectedAnswers m)
            newSet
              | isMultiple q = if S.member opt cur
                                 then S.delete opt cur
                                 else S.insert opt cur
              | otherwise    = S.singleton opt
            newAnswers = M.insert qid newSet (selectedAnswers m)
            newModel   = m{selectedAnswers = newAnswers}
         in newModel <# do
              let js = "localStorage.setItem('"
                        <> ms storageKey <> "', '"
                        <> ms (serializeAnswers newAnswers) <> "')"
              _ <- eval js
              return NoOp

  SetQuestions qs correct expls -> \_ ->
    batchEff
      (Model qs 0 M.empty correct expls False)
      [ do
          val <- eval (ms ("localStorage.getItem('" <> storageKey <> "') || ''"))
          str <- valToStr val
          return (LoadAnswers (JS.unpack str))
      ]

  LoadAnswers raw -> \m ->
    noEff $ m{selectedAnswers = deserializeAnswers raw}

  Evaluate   -> \m -> noEff $ m{showResults = True}
  BackToQuiz -> \m -> noEff $ m{showResults = False}
  NoOp       -> noEff

-- ── Helpers ────────────────────────────────────────────────────────────────

currentQuestionData :: Model -> Maybe Question
currentQuestionData m
  | null (questionsList m)                       = Nothing
  | currentQuestion m >= length (questionsList m) = Nothing   -- last page
  | otherwise = Just (questionsList m !! currentQuestion m)

isOnLastPage :: Model -> Bool
isOnLastPage m = currentQuestion m >= length (questionsList m)

questionIsCorrect :: Model -> Question -> Bool
questionIsCorrect m q =
  let sel  = M.findWithDefault S.empty (questionId q) (selectedAnswers m)
      corr = M.findWithDefault S.empty (questionId q) (correctAnswers  m)
   in not (S.null corr) && sel == corr

-- ── Top-level view dispatcher ──────────────────────────────────────────────

viewModel :: Model -> View Action
viewModel m
  | showResults m          = viewResults m
  | null (questionsList m) = div_ [style_ containerSt] [p_ [] ["Loading…"]]
  | isOnLastPage m         = viewLastPage m
  | otherwise              = case currentQuestionData m of
      Nothing -> div_ [style_ containerSt] [p_ [] ["Loading…"]]
      Just q  -> viewQuizPage m q

-- ── Quiz page ──────────────────────────────────────────────────────────────

viewQuizPage :: Model -> Question -> View Action
viewQuizPage m q =
  div_ [style_ containerSt]
    [ h1_ [style_ headingSt] ["Quiz"]
    , p_ [style_ countSt]
        [text $ ms $ "Question " <> show (questionId q)
                                 <> " of "
                                 <> show (length (questionsList m))]
    , div_ [style_ questionSt] [text (questionText q)]
    , if questionCode q == ""
        then text ""
        else pre_ [style_ codeSt] [text (questionCode q)]
    , if isMultiple q
        then p_ [style_ hintSt] ["(Select all that apply)"]
        else text ""
    , div_ [style_ tableSt] (map (renderOption m q) (questionOptions q))
    , div_ [style_ navSt]
        [ button_ [onClick PrevQuestion, style_ btnSt]      ["← Previous"]
        , button_ [onClick NextQuestion, style_ btnSt]      ["Next →"]
        , button_ [onClick Evaluate,     style_ evalBtnSt]  ["✓ Evaluate"]
        ]
    ]

-- ── Last page ──────────────────────────────────────────────────────────────

viewLastPage :: Model -> View Action
viewLastPage m =
  let total    = length (questionsList m)
      answered = M.size (selectedAnswers m)
   in div_ [style_ containerSt]
        [ h1_ [style_ headingSt] ["Quiz Complete"]
        , div_ [style_ lastBoxSt]
            [ p_ [style_ lastTextSt]
                [ text $ ms $
                    "You have answered " <> show answered
                    <> " out of " <> show total <> " questions." ]
            , p_ [style_ lastHintSt]
                ["Click \"Evaluate\" to see your score."]
            ]
        , div_ [style_ navSt]
            [ button_ [onClick PrevQuestion, style_ btnSt]     ["← Previous"]
            , button_ [onClick Evaluate,     style_ evalBtnSt] ["✓ Evaluate Answers"]
            ]
        ]

-- ── Results page ───────────────────────────────────────────────────────────

viewResults :: Model -> View Action
viewResults m =
  let qs      = questionsList m
      total   = length qs
      numCorr = length $ filter (questionIsCorrect m) qs
      pct     = if total == 0 then 0 else (numCorr * 100) `div` total
   in div_ [style_ containerSt]
        [ h1_ [style_ headingSt] ["Results"]
        , div_ [style_ (scoreBannerSt pct)]
            [ p_ [style_ scoreLabelSt]
                [text $ ms $ show numCorr <> " / " <> show total <> " correct"]
            , p_ [style_ pctSt]
                [text $ ms $ show pct <> "%"]
            ]
        , div_ [style_ resultGridSt] (map (renderResultCell m) qs)
        , div_ [style_ navSt]
            [ button_ [onClick BackToQuiz, style_ btnSt] ["← Back to Quiz"] ]
        ]

renderResultCell :: Model -> Question -> View Action
renderResultCell m q =
  let sel        = M.findWithDefault S.empty (questionId q) (selectedAnswers m)
      corr       = M.findWithDefault S.empty (questionId q) (correctAnswers  m)
      expl       = M.findWithDefault ""      (questionId q) (explanations    m)
      unanswered = S.null sel
      ok         = questionIsCorrect m q
      cellSt
        | unanswered = resultCellUnansweredSt
        | ok         = resultCellCorrectSt
        | otherwise  = resultCellWrongSt
      mark :: String
      mark
        | unanswered = "–"
        | ok         = "✓"
        | otherwise  = "✗"
   in div_ [style_ cellSt, textProp "title" (ms expl)]
        [ div_ [style_ cellNumSt]  [text $ ms $ "Q" <> show (questionId q)]
        , div_ [style_ cellMarkSt] [text $ ms mark]
        , if not ok && not unanswered
            then div_ [style_ cellDetailSt]
                   [ text $ ms $ "You: "  <> S.toList sel
                   , text "  "
                   , text $ ms $ "Ans: " <> S.toList corr
                   ]
            else text ""
        , if not (null expl)
            then div_ [style_ cellHintSt] ["ⓘ"]
            else text ""
        ]

-- ── Shared option renderer ─────────────────────────────────────────────────

renderOption :: Model -> Question -> (Char, MisoString) -> View Action
renderOption m q (opt, labelText) =
  let sel       = M.findWithDefault S.empty (questionId q) (selectedAnswers m)
      isChecked = S.member opt sel
      inType    = if isMultiple q then "checkbox" else "radio"
      rName     = ms ("q" <> show (questionId q))
      rowSt
        | isChecked = M.fromList [("background", "#e8f4fd")] <> rowBaseSt
        | otherwise = rowBaseSt
   in div_ [style_ rowSt]
        [ div_ [style_ inputCellSt]
            [input_ [type_ inType, name_ rName, checked_ isChecked, onClick (ToggleAnswer opt)]]
        , div_ [style_ letterCellSt] [text $ ms [opt]]
        , div_ [style_ textCellSt]   [text labelText]
        ]

-- ── Styles ─────────────────────────────────────────────────────────────────

type St = M.Map MisoString MisoString

containerSt :: St
containerSt = M.fromList
  [ ("max-width","860px"), ("margin","0 auto"), ("padding","24px")
  , ("font-family","system-ui, sans-serif"), ("color","#222") ]

headingSt :: St
headingSt = M.fromList [("margin-bottom","4px"), ("color","#1a56db")]

countSt :: St
countSt = M.fromList [("color","#6b7280"), ("font-size","0.9em"), ("margin","0 0 12px")]

questionSt :: St
questionSt = M.fromList
  [ ("font-size","1.05em"), ("line-height","1.6")
  , ("margin-bottom","16px"), ("white-space","pre-wrap") ]

hintSt :: St
hintSt = M.fromList [("color","#6b7280"), ("font-size","0.85em"), ("margin","-8px 0 10px")]

codeSt :: St
codeSt = M.fromList
  [ ("background","#f6f8fa"), ("border","1px solid #d0d7de")
  , ("border-radius","6px"), ("padding","14px 16px")
  , ("font-family","ui-monospace, monospace"), ("font-size","0.88em")
  , ("line-height","1.5"), ("overflow-x","auto")
  , ("margin-bottom","16px"), ("white-space","pre") ]

tableSt :: St
tableSt = M.fromList
  [ ("display","table"), ("width","100%")
  , ("border-collapse","separate"), ("border-spacing","0 4px")
  , ("margin-bottom","16px") ]

rowBaseSt :: St
rowBaseSt = M.fromList
  [("display","table-row"), ("border-radius","4px"), ("cursor","pointer")]

inputCellSt :: St
inputCellSt = M.fromList
  [ ("display","table-cell"), ("padding","8px 6px"), ("width","32px")
  , ("vertical-align","middle"), ("text-align","center") ]

letterCellSt :: St
letterCellSt = M.fromList
  [ ("display","table-cell"), ("padding","8px 6px"), ("width","36px")
  , ("font-weight","600"), ("vertical-align","middle") ]

textCellSt :: St
textCellSt = M.fromList
  [ ("display","table-cell"), ("padding","8px 12px")
  , ("vertical-align","middle"), ("line-height","1.4") ]

navSt :: St
navSt = M.fromList
  [("display","flex"), ("gap","12px"), ("margin-top","20px"), ("flex-wrap","wrap")]

btnSt :: St
btnSt = M.fromList
  [ ("padding","10px 24px"), ("font-size","1em"), ("cursor","pointer")
  , ("background","#1a56db"), ("color","#fff")
  , ("border","none"), ("border-radius","6px") ]

evalBtnSt :: St
evalBtnSt = M.fromList
  [ ("padding","10px 24px"), ("font-size","1em"), ("cursor","pointer")
  , ("background","#16a34a"), ("color","#fff")
  , ("border","none"), ("border-radius","6px"), ("margin-left","auto") ]

lastBoxSt :: St
lastBoxSt = M.fromList
  [ ("background","#f0f9ff"), ("border","1px solid #bae6fd")
  , ("border-radius","8px"), ("padding","24px"), ("margin","24px 0")
  , ("text-align","center") ]

lastTextSt :: St
lastTextSt = M.fromList [("font-size","1.1em"), ("margin-bottom","8px")]

lastHintSt :: St
lastHintSt = M.fromList [("color","#6b7280"), ("font-size","0.95em")]

scoreBannerSt :: Int -> St
scoreBannerSt pct =
  let (bg, bord)
        | pct >= 80 = ("#dcfce7", "#86efac")
        | pct >= 60 = ("#fef9c3", "#fde047")
        | otherwise = ("#fee2e2", "#fca5a5")
   in M.fromList
        [ ("background",bg), ("border","1px solid " <> bord)
        , ("border-radius","8px"), ("padding","20px 24px")
        , ("margin-bottom","24px"), ("text-align","center") ]

scoreLabelSt :: St
scoreLabelSt = M.fromList
  [("font-size","1.3em"), ("font-weight","600"), ("margin","0 0 4px")]

pctSt :: St
pctSt = M.fromList [("font-size","3em"), ("font-weight","700"), ("margin","0")]

resultGridSt :: St
resultGridSt = M.fromList
  [ ("display","grid")
  , ("grid-template-columns","repeat(auto-fill, minmax(110px, 1fr))")
  , ("gap","8px"), ("margin-bottom","24px") ]

resultCellBaseSt :: St
resultCellBaseSt = M.fromList
  [("border-radius","6px"), ("padding","8px"), ("font-size","0.85em"), ("text-align","center")]

resultCellCorrectSt :: St
resultCellCorrectSt =
  M.fromList [("background","#dcfce7"), ("border","1px solid #86efac")] <> resultCellBaseSt

resultCellWrongSt :: St
resultCellWrongSt =
  M.fromList [("background","#fee2e2"), ("border","1px solid #fca5a5")] <> resultCellBaseSt

resultCellUnansweredSt :: St
resultCellUnansweredSt =
  M.fromList [("background","#f3f4f6"), ("border","1px solid #d1d5db")] <> resultCellBaseSt

cellNumSt :: St
cellNumSt = M.fromList [("font-weight","600"), ("font-size","0.9em")]

cellMarkSt :: St
cellMarkSt = M.fromList [("font-size","1.1em"), ("margin","2px 0")]

cellDetailSt :: St
cellDetailSt = M.fromList [("font-size","0.75em"), ("color","#6b7280"), ("margin-top","4px")]

cellHintSt :: St
cellHintSt = M.fromList
  [ ("font-size","0.75em"), ("color","#9ca3af"), ("margin-top","4px")
  , ("cursor","help") ]

