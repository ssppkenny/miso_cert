{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Miso
import Miso.Html.Element
import Miso.Html.Event      (onClick)
import Miso.Html.Property   (type_, name_, checked_, class_)
import qualified Miso.CSS   as CSS

import Control.Monad.State.Class (get, put, modify)
import Data.List                 (dropWhileEnd, intercalate, isInfixOf)
import Data.Maybe                (mapMaybe)
import Text.Read                 (readMaybe)
import Data.Char                 (isDigit, isUpper, toLower)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

-- ── Storage key ────────────────────────────────────────────────────────────

storageKey :: MisoString
storageKey = "cert-quiz-answers"

-- ── Domain types ───────────────────────────────────────────────────────────

data Question = Question
  { questionId      :: Int
  , questionText    :: MisoString
  , questionCode    :: MisoString
  , questionOptions :: [(Char, MisoString)]
  , isMultiple      :: Bool
  } deriving (Show, Eq)

data Model = Model
  { questionsList   :: [Question]
  , currentQuestion :: Int
  , selectedAnswers :: M.Map Int (S.Set Char)
  , correctAnswers  :: M.Map Int (S.Set Char)
  , explanations    :: M.Map Int String
  , showResults     :: Bool
  , loadingError    :: Maybe MisoString
  } deriving (Show, Eq)

initialModel :: Model
initialModel = Model
  { questionsList   = []
  , currentQuestion = 0
  , selectedAnswers = M.empty
  , correctAnswers  = M.empty
  , explanations    = M.empty
  , showResults     = False
  , loadingError    = Nothing
  }

data Action
  = NextQuestion
  | PrevQuestion
  | ToggleAnswer Char
  | StartFetch
  | GotQuestionsText String
  | GotAnswersText String String
  | FetchFailed String
  | LoadSavedAnswers String
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
      bodyStr = dropWhile (== ' ') (drop 1 rest)
   in (number, bodyStr)

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
    _                             -> Nothing

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

parseCorrectAnswers :: [String] -> M.Map Int (S.Set Char)
parseCorrectAnswers ls = M.fromList $ mapMaybe parseLine ls
 where
  parseLine s
    | startsWithNumberDot s =
        let (qNum, rest)    = parseNumberDot s
            (letterPart, _) = break (== '.') rest
            letters         = filter isUpper letterPart
         in if null letters then Nothing
            else Just (qNum, S.fromList letters)
    | otherwise = Nothing

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
  extractExpl afterNum contLines =
    let afterDot = drop 1 (dropWhile (/= '.') afterNum)
        firstBit = dropWhile (== ' ') afterDot
        allLines = firstBit : contLines
     in unwords $ filter (not . null) $ map (dropWhile (== ' ')) allLines

-- ── Multiple-choice detection ──────────────────────────────────────────────

detectMultiple :: String -> Bool
detectMultiple s =
  any (`isInfixOf` map toLower s)
    ["choose two", "choose three", "choose four", "choose five"]

-- ── localStorage serialisation ─────────────────────────────────────────────

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

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main :: IO ()
#ifdef INTERACTIVE
main = reload (startApp defaultEvents app)
#else
main = startApp defaultEvents app
#endif

app :: App Model Action
app = (component initialModel updateModel viewModel)
  { mount = Just StartFetch }

-- ── Update ─────────────────────────────────────────────────────────────────

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  StartFetch ->
    getText "/questions1.txt" []
      (\resp -> GotQuestionsText (fromMisoString (body resp)))
      ((\_ -> FetchFailed "Failed to load questions1.txt") :: Response MisoString -> Action)

  GotQuestionsText questTxt ->
    getText "/answers1.txt" []
      (\resp -> GotAnswersText questTxt (fromMisoString (body resp)))
      ((\_ -> FetchFailed "Failed to load answers1.txt") :: Response MisoString -> Action)

  GotAnswersText questTxt ansTxt -> do
    let mylines   = lines questTxt
        questions = readQuestions mylines
        ans       = readAnswers   mylines
        codes     = readCodeBlocks mylines
        qs        = buildQuiz questions ans codes
        correct   = parseCorrectAnswers (lines ansTxt)
        expls     = parseExplanations   (lines ansTxt)
        newModel  = initialModel
          { questionsList  = qs
          , correctAnswers = correct
          , explanations   = expls
          }
    put newModel
    io $ do
      result <- (getLocalStorage storageKey :: IO (Either MisoString String))
      return $ case result of
        Right raw -> LoadSavedAnswers raw
        Left  _   -> LoadSavedAnswers ""

  FetchFailed err ->
    modify $ \m -> m { loadingError = Just (ms err) }

  LoadSavedAnswers raw ->
    modify $ \m -> m { selectedAnswers = deserializeAnswers raw }

  NextQuestion ->
    modify $ \m -> m { currentQuestion =
      min (length (questionsList m)) (currentQuestion m + 1) }

  PrevQuestion ->
    modify $ \m -> m { currentQuestion =
      max 0 (currentQuestion m - 1) }

  ToggleAnswer opt -> do
    m <- get
    case currentQuestionData m of
      Nothing -> pure ()
      Just q  ->
        let qid      = questionId q
            cur      = M.findWithDefault S.empty qid (selectedAnswers m)
            newSet
              | isMultiple q = if S.member opt cur
                                 then S.delete opt cur
                                 else S.insert opt cur
              | otherwise    = S.singleton opt
            newAnswers = M.insert qid newSet (selectedAnswers m)
        in do
          put m { selectedAnswers = newAnswers }
          io_ $ setLocalStorage storageKey (serializeAnswers newAnswers :: String)

  Evaluate   -> modify $ \m -> m { showResults = True  }
  BackToQuiz -> modify $ \m -> m { showResults = False }
  NoOp       -> pure ()

-- ── Helpers ────────────────────────────────────────────────────────────────

currentQuestionData :: Model -> Maybe Question
currentQuestionData m
  | null (questionsList m)                        = Nothing
  | currentQuestion m >= length (questionsList m) = Nothing
  | otherwise = Just (questionsList m Prelude.!! currentQuestion m)

isOnLastPage :: Model -> Bool
isOnLastPage m = currentQuestion m >= length (questionsList m)

questionIsCorrect :: Model -> Question -> Bool
questionIsCorrect m q =
  let sel  = M.findWithDefault S.empty (questionId q) (selectedAnswers m)
      corr = M.findWithDefault S.empty (questionId q) (correctAnswers  m)
   in not (S.null corr) && sel == corr

-- ── Top-level view dispatcher ──────────────────────────────────────────────

viewModel :: Model -> View Model Action
viewModel m
  | showResults m            = viewResults m
  | Just err <- loadingError m =
      div_ [CSS.style_ containerSt]
        [ h1_ [CSS.style_ headingSt] ["Error"]
        , p_ [CSS.style_ ["color" =: "#dc2626"]] [text err]
        ]
  | null (questionsList m)   = div_ [CSS.style_ containerSt] [p_ [] ["Loading…"]]
  | isOnLastPage m           = viewLastPage m
  | otherwise                = case currentQuestionData m of
      Nothing -> div_ [CSS.style_ containerSt] [p_ [] ["Loading…"]]
      Just q  -> viewQuizPage m q

-- ── Quiz page ──────────────────────────────────────────────────────────────

viewQuizPage :: Model -> Question -> View Model Action
viewQuizPage m q =
  div_ [CSS.style_ containerSt]
    [ h1_ [CSS.style_ headingSt] ["Quiz"]
    , p_ [CSS.style_ countSt]
        [text $ ms $ "Question " <> show (questionId q)
                                 <> " of "
                                 <> show (length (questionsList m))]
    , div_ [CSS.style_ questionSt] [text (questionText q)]
    , if questionCode q == ""
        then text ""
        else pre_ [CSS.style_ codeSt] [text (questionCode q)]
    , if isMultiple q
        then p_ [CSS.style_ hintSt] ["(Select all that apply)"]
        else text ""
    , div_ [CSS.style_ tableSt] (map (renderOption m q) (questionOptions q))
    , div_ [CSS.style_ navSt]
        [ button_ [onClick PrevQuestion, CSS.style_ btnSt]     ["← Previous"]
        , button_ [onClick NextQuestion, CSS.style_ btnSt]     ["Next →"]
        , button_ [onClick Evaluate,     CSS.style_ evalBtnSt] ["✓ Evaluate"]
        ]
    ]

-- ── Last page ──────────────────────────────────────────────────────────────

viewLastPage :: Model -> View Model Action
viewLastPage m =
  let total    = length (questionsList m)
      answered = M.size (selectedAnswers m)
   in div_ [CSS.style_ containerSt]
        [ h1_ [CSS.style_ headingSt] ["Quiz Complete"]
        , div_ [CSS.style_ lastBoxSt]
            [ p_ [CSS.style_ lastTextSt]
                [ text $ ms $
                    "You have answered " <> show answered
                    <> " out of " <> show total <> " questions." ]
            , p_ [CSS.style_ lastHintSt]
                ["Click \"Evaluate\" to see your score."]
            ]
        , div_ [CSS.style_ navSt]
            [ button_ [onClick PrevQuestion, CSS.style_ btnSt]     ["← Previous"]
            , button_ [onClick Evaluate,     CSS.style_ evalBtnSt] ["✓ Evaluate Answers"]
            ]
        ]

-- ── Results page ───────────────────────────────────────────────────────────

viewResults :: Model -> View Model Action
viewResults m =
  let qs      = questionsList m
      total   = length qs
      numCorr = length $ filter (questionIsCorrect m) qs
      pct     = if total == 0 then 0 else (numCorr * 100) `div` total
   in div_ [CSS.style_ containerSt]
        [ h1_ [CSS.style_ headingSt] ["Results"]
        , div_ [CSS.style_ (scoreBannerSt pct)]
            [ p_ [CSS.style_ scoreLabelSt]
                [text $ ms $ show numCorr <> " / " <> show total <> " correct"]
            , p_ [CSS.style_ pctSt]
                [text $ ms $ show pct <> "%"]
            ]
        , div_ [CSS.style_ resultGridSt] (map (renderResultCell m) qs)
        , div_ [CSS.style_ navSt]
            [ button_ [onClick BackToQuiz, CSS.style_ btnSt] ["← Back to Quiz"] ]
        ]

renderResultCell :: Model -> Question -> View Model Action
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
   in div_ [CSS.style_ cellSt, textProp "title" (ms expl)]
        [ div_ [CSS.style_ cellNumSt]  [text $ ms $ "Q" <> show (questionId q)]
        , div_ [CSS.style_ cellMarkSt] [text $ ms mark]
        , if not ok && not unanswered
            then div_ [CSS.style_ cellDetailSt]
                   [ text $ ms $ "You: "  <> S.toList sel
                   , text "  "
                   , text $ ms $ "Ans: " <> S.toList corr
                   ]
            else text ""
        , if not (null expl)
            then div_ [CSS.style_ cellHintSt] ["ⓘ"]
            else text ""
        ]

-- ── Shared option renderer ─────────────────────────────────────────────────

renderOption :: Model -> Question -> (Char, MisoString) -> View Model Action
renderOption m q (opt, labelText) =
  let sel       = M.findWithDefault S.empty (questionId q) (selectedAnswers m)
      isChecked = S.member opt sel
      inType    = if isMultiple q then "checkbox" else "radio"
      rName     = ms ("q" <> show (questionId q))
      rowSt
        | isChecked = ("background" =: "#e8f4fd") : rowBaseSt
        | otherwise = rowBaseSt
   in div_ [CSS.style_ rowSt]
        [ div_ [CSS.style_ inputCellSt]
            [input_ [type_ inType, name_ rName, checked_ isChecked, onClick (ToggleAnswer opt)]]
        , div_ [CSS.style_ letterCellSt] [text $ ms [opt]]
        , div_ [CSS.style_ textCellSt]   [text labelText]
        ]

-- ── Styles ─────────────────────────────────────────────────────────────────

type St = [CSS.Style]

containerSt :: St
containerSt =
  [ "max-width"   =: "860px",  "margin"      =: "0 auto"
  , "padding"     =: "24px",   "font-family" =: "system-ui, sans-serif"
  , "color"       =: "#222" ]

headingSt :: St
headingSt = ["margin-bottom" =: "4px", "color" =: "#1a56db"]

countSt :: St
countSt = ["color" =: "#6b7280", "font-size" =: "0.9em", "margin" =: "0 0 12px"]

questionSt :: St
questionSt =
  [ "font-size"     =: "1.05em", "line-height"    =: "1.6"
  , "margin-bottom" =: "16px",   "white-space"    =: "pre-wrap" ]

hintSt :: St
hintSt = ["color" =: "#6b7280", "font-size" =: "0.85em", "margin" =: "-8px 0 10px"]

codeSt :: St
codeSt =
  [ "background"    =: "#f6f8fa", "border"        =: "1px solid #d0d7de"
  , "border-radius" =: "6px",     "padding"       =: "14px 16px"
  , "font-family"   =: "ui-monospace, monospace"
  , "font-size"     =: "0.88em",  "line-height"   =: "1.5"
  , "overflow-x"    =: "auto",    "margin-bottom" =: "16px"
  , "white-space"   =: "pre" ]

tableSt :: St
tableSt =
  [ "display"          =: "table",   "width"          =: "100%"
  , "border-collapse"  =: "separate","border-spacing" =: "0 4px"
  , "margin-bottom"    =: "16px" ]

rowBaseSt :: St
rowBaseSt =
  ["display" =: "table-row", "border-radius" =: "4px", "cursor" =: "pointer"]

inputCellSt :: St
inputCellSt =
  [ "display" =: "table-cell", "padding" =: "8px 6px", "width" =: "32px"
  , "vertical-align" =: "middle", "text-align" =: "center" ]

letterCellSt :: St
letterCellSt =
  [ "display" =: "table-cell", "padding" =: "8px 6px", "width" =: "36px"
  , "font-weight" =: "600", "vertical-align" =: "middle" ]

textCellSt :: St
textCellSt =
  [ "display" =: "table-cell", "padding" =: "8px 12px"
  , "vertical-align" =: "middle", "line-height" =: "1.4" ]

navSt :: St
navSt =
  ["display" =: "flex", "gap" =: "12px", "margin-top" =: "20px", "flex-wrap" =: "wrap"]

btnSt :: St
btnSt =
  [ "padding" =: "10px 24px", "font-size" =: "1em",   "cursor"        =: "pointer"
  , "background" =: "#1a56db", "color"    =: "#fff"
  , "border" =: "none",        "border-radius" =: "6px" ]

evalBtnSt :: St
evalBtnSt =
  [ "padding" =: "10px 24px", "font-size" =: "1em",   "cursor"        =: "pointer"
  , "background" =: "#16a34a", "color"    =: "#fff"
  , "border" =: "none",        "border-radius" =: "6px", "margin-left" =: "auto" ]

lastBoxSt :: St
lastBoxSt =
  [ "background"    =: "#f0f9ff", "border"      =: "1px solid #bae6fd"
  , "border-radius" =: "8px",     "padding"     =: "24px"
  , "margin"        =: "24px 0",  "text-align"  =: "center" ]

lastTextSt :: St
lastTextSt = ["font-size" =: "1.1em", "margin-bottom" =: "8px"]

lastHintSt :: St
lastHintSt = ["color" =: "#6b7280", "font-size" =: "0.95em"]

scoreBannerSt :: Int -> St
scoreBannerSt pct =
  let (bg, bord)
        | pct >= 80 = ("#dcfce7", "#86efac")
        | pct >= 60 = ("#fef9c3", "#fde047")
        | otherwise = ("#fee2e2", "#fca5a5")
   in [ "background"    =: bg
      , "border"        =: ("1px solid " <> bord)
      , "border-radius" =: "8px"
      , "padding"       =: "20px 24px"
      , "margin-bottom" =: "24px"
      , "text-align"    =: "center" ]

scoreLabelSt :: St
scoreLabelSt = ["font-size" =: "1.3em", "font-weight" =: "600", "margin" =: "0 0 4px"]

pctSt :: St
pctSt = ["font-size" =: "3em", "font-weight" =: "700", "margin" =: "0"]

resultGridSt :: St
resultGridSt =
  [ "display"                =: "grid"
  , "grid-template-columns"  =: "repeat(auto-fill, minmax(110px, 1fr))"
  , "gap"                    =: "8px"
  , "margin-bottom"          =: "24px" ]

resultCellBaseSt :: St
resultCellBaseSt =
  ["border-radius" =: "6px", "padding" =: "8px", "font-size" =: "0.85em", "text-align" =: "center"]

resultCellCorrectSt :: St
resultCellCorrectSt =
  ["background" =: "#dcfce7", "border" =: "1px solid #86efac"] ++ resultCellBaseSt

resultCellWrongSt :: St
resultCellWrongSt =
  ["background" =: "#fee2e2", "border" =: "1px solid #fca5a5"] ++ resultCellBaseSt

resultCellUnansweredSt :: St
resultCellUnansweredSt =
  ["background" =: "#f3f4f6", "border" =: "1px solid #d1d5db"] ++ resultCellBaseSt

cellNumSt :: St
cellNumSt = ["font-weight" =: "600", "font-size" =: "0.9em"]

cellMarkSt :: St
cellMarkSt = ["font-size" =: "1.1em", "margin" =: "2px 0"]

cellDetailSt :: St
cellDetailSt = ["font-size" =: "0.75em", "color" =: "#6b7280", "margin-top" =: "4px"]

cellHintSt :: St
cellHintSt =
  ["font-size" =: "0.75em", "color" =: "#9ca3af", "margin-top" =: "4px", "cursor" =: "help"]
