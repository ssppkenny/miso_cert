{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Miso
import qualified Miso.CSS as CSS
import Miso.Html.Element
import Miso.Html.Event (onClick)
import Miso.Html.Property (checked_, class_, name_, src_, type_)

import Control.Monad.State.Class (get, modify, put)
import Data.Char (isDigit, isUpper, toLower)
import Data.List (dropWhileEnd, intercalate, isInfixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Text.Read (readMaybe)

-- ── Storage key ────────────────────────────────────────────────────────────

storageKey :: Int -> MisoString
storageKey n = ms $ "cert-quiz-ch" <> show n

-- ── Domain types ───────────────────────────────────────────────────────────

data Question = Question
  { questionId :: Int
  , questionText :: MisoString
  , questionCode :: MisoString
  , questionOptions :: [(Char, MisoString)]
  , isMultiple :: Bool
  }
  deriving (Show, Eq)

data AppPage
  = ChapterSelectionPage
  | LoadingPage
  | QuizPage
  | ResultsPage
  deriving (Show, Eq)

data ChapterInfo = ChapterInfo
  { chapterNum :: Int
  , chapterName :: MisoString
  }
  deriving (Show, Eq)

data Model = Model
  { questionsList :: [Question]
  , currentQuestion :: Int
  , selectedAnswers :: M.Map Int (S.Set Char)
  , correctAnswers :: M.Map Int (S.Set Char)
  , explanations :: M.Map Int String
  , appPage :: AppPage
  , loadingError :: Maybe MisoString
  , availableChapters :: [ChapterInfo]
  , currentChapter :: Maybe Int
  , chapterImages :: S.Set String   -- filenames present: "25.png", "142_A.png", …
  }
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    { questionsList = []
    , currentQuestion = 0
    , selectedAnswers = M.empty
    , correctAnswers = M.empty
    , explanations = M.empty
    , appPage = ChapterSelectionPage
    , loadingError = Nothing
    , availableChapters = []
    , currentChapter = Nothing
    , chapterImages = S.empty
    }

data Action
  = NextQuestion
  | PrevQuestion
  | ToggleAnswer Char
  | StartFetch
  | GotChapterList String
  | SelectChapter Int
  | GotQuestionsText Int String
  | GotAnswersText Int String String
  | GotImageList String            -- image manifest text (empty = none)
  | FetchFailed String
  | LoadSavedAnswers String
  | Evaluate
  | BackToQuiz
  | BackToChapterSelection
  | ResetAnswers
  | NoOp
  deriving (Show, Eq)

-- ── Parsing: questions1.txt ────────────────────────────────────────────────

startsWithLetterDot :: String -> Bool
startsWithLetterDot str =
  case span isUpper str of
    ("", _) -> False
    (_, '.' : _) -> True
    _ -> False

startsWithNumberDot :: String -> Bool
startsWithNumberDot str =
  case span isDigit str of
    ("", _) -> False
    (_, '.' : ' ' : _) -> True -- "N.  A." format
    (_, '.' : '\t' : _) -> True
    _ -> False


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

readAnswers :: [String] -> M.Map Int (M.Map Char String)
readAnswers mylines = M.fromList $ zip [0 ..] (go mylines False Nothing [] [])
 where
  -- go :: remaining lines -> inAnswers -> currentLetter -> accumulated text lines for current letter -> accumulated (letter,text) pairs for current question
  go [] _ curL curT acc =
    let grp = finishGrp curL curT acc
     in [M.fromList grp | not (null grp)]
  go (l : rest) inAnswers curL curT acc
    | startsWithNumberDot l =
        let grp = finishGrp curL curT acc
         in if null grp
              then go rest False Nothing [] []
              else M.fromList grp : go rest False Nothing [] []
    | Just (c, txt) <- parseAnswerLine l =
        let acc' = maybe acc (\prevC -> (prevC, joinLines curT) : acc) curL
         in go rest True (Just c) [txt] acc'
    | inAnswers, Just _ <- curL, not (null (trimL l)) =
        go rest True curL (curT ++ [trimL l]) acc
    | otherwise = go rest inAnswers curL curT acc

  parseAnswerLine (c : '.' : rest) | isUpper c = Just (c, dropWhile (== ' ') rest)
  parseAnswerLine _ = Nothing

  trimL = dropWhile (== ' ')

  joinLines ts = unlines (filter (not . null) ts)

  finishGrp Nothing _ acc = reverse acc
  finishGrp (Just c) ts acc = reverse ((c, joinLines ts) : acc)

readQuestions :: [String] -> (M.Map Int String, [Int])
readQuestions ls =
  let linesWithNumbers = linesWithNumbersDot ls
      m = map (\x -> (fst x, parseNumberDot $ snd x)) linesWithNumbers
   in getQuestions m

readCodeBlocks :: [String] -> M.Map Int MisoString
readCodeBlocks ls = M.fromList $ go ls
 where
  go [] = []
  go (l : rest)
    | startsWithNumberDot l =
        let (qNum, _) = parseNumberDot l
            codeLines = takeWhile isCodeLine rest
            trimmed = dropWhileEnd null codeLines
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
        let (qNum, rest) = parseNumberDot s
            (letterPart, _) = break (== '.') rest
            letters = filter isUpper letterPart
         in if null letters
              then Nothing
              else Just (qNum, S.fromList letters)
    | otherwise = Nothing

parseExplanations :: [String] -> M.Map Int String
parseExplanations ls = M.fromList $ go ls
 where
  go [] = []
  go (l : rest)
    | startsWithNumberDot l =
        let (qNum, afterNum) = parseNumberDot l
            contLines = takeWhile (not . startsWithNumberDot) rest
            remaining = dropWhile (not . startsWithNumberDot) rest
            expl = extractExpl afterNum contLines
         in (qNum, expl) : go remaining
    | otherwise = go rest
  extractExpl afterNum contLines =
    let afterDot = drop 1 (dropWhile (/= '.') afterNum)
        firstBit = dropWhile (== ' ') afterDot
        allLines = firstBit : contLines
     in unwords $ filter (not . null) $ map (dropWhile (== ' ')) allLines

-- ── Chapter list parsing ───────────────────────────────────────────────────

parseChapterList :: String -> [ChapterInfo]
parseChapterList s = mapMaybe parseLine (filter (not . null) (lines s))
 where
  parseLine l = case break (== ':') l of
    (numStr, ':' : name) ->
      (\n -> ChapterInfo n (ms (trim name))) <$> (readMaybe numStr :: Maybe Int)
    _ -> Nothing
  trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- ── Image manifest parsing ─────────────────────────────────────────────────

-- | Parse images.txt — one filename per line (e.g. "25.png", "142_A.png").
parseImageList :: String -> S.Set String
parseImageList = S.fromList . filter (not . null) . lines

-- ── Multiple-choice detection ──────────────────────────────────────────────

detectMultiple :: String -> Bool
detectMultiple s =
  any
    (`isInfixOf` map toLower s)
    ["choose two", "choose three", "choose four", "choose five"]

-- ── localStorage serialisation ─────────────────────────────────────────────

serializeAnswers :: M.Map Int (S.Set Char) -> String
serializeAnswers m =
  intercalate "|" $
    map (\(k, v) -> show k <> ":" <> S.toList v) (M.toList m)

deserializeAnswers :: String -> M.Map Int (S.Set Char)
deserializeAnswers "" = M.empty
deserializeAnswers s = M.fromList $ mapMaybe parseEntry (splitOn '|' s)
 where
  parseEntry entry = case break (== ':') entry of
    (numStr, ':' : chars) ->
      (\k -> (k, S.fromList chars)) <$> (readMaybe numStr :: Maybe Int)
    _ -> Nothing
  splitOn _ "" = [""]
  splitOn c (x : xs)
    | x == c = "" : splitOn c xs
    | otherwise = let (hd : tl) = splitOn c xs in (x : hd) : tl

-- ── Build quiz ─────────────────────────────────────────────────────────────

buildQuiz ::
  (M.Map Int String, [Int]) ->
  M.Map Int (M.Map Char String) ->
  M.Map Int MisoString ->
  [Question]
buildQuiz (qMap, _) aMap codeMap = map toQuestion (M.toAscList qMap)
 where
  toQuestion (qNum, qText) =
    let options = M.findWithDefault M.empty (qNum - 1) aMap
        code = M.findWithDefault "" qNum codeMap
     in Question
          { questionId = qNum
          , questionText = ms qText
          , questionCode = code
          , questionOptions = map (\(k, v) -> (k, ms v)) (M.toAscList options)
          , isMultiple = detectMultiple qText
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
app =
  (component initialModel updateModel viewModel)
    { mount = Just StartFetch
    }

-- ── Update ─────────────────────────────────────────────────────────────────

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  StartFetch ->
    getText
      "/resources/chapters.txt"
      []
      (\resp -> GotChapterList (fromMisoString (body resp)))
      ((\_ -> FetchFailed "Failed to load chapters.txt") :: Response MisoString -> Action)
  GotChapterList txt ->
    modify $ \m -> m{availableChapters = parseChapterList txt, appPage = ChapterSelectionPage}
  SelectChapter n -> do
    modify $ \m -> m{appPage = LoadingPage, loadingError = Nothing}
    getText
      (ms $ "/resources/chapter" <> show n <> "/questions.txt")
      []
      (\resp -> GotQuestionsText n (fromMisoString (body resp)))
      ((\_ -> FetchFailed $ "Failed to load questions for chapter " <> show n) :: Response MisoString -> Action)
  GotQuestionsText n questTxt ->
    getText
      (ms $ "/resources/chapter" <> show n <> "/answers.txt")
      []
      (\resp -> GotAnswersText n questTxt (fromMisoString (body resp)))
      ((\_ -> FetchFailed $ "Failed to load answers for chapter " <> show n) :: Response MisoString -> Action)
  GotAnswersText n questTxt ansTxt -> do
    let mylines = lines questTxt
        questions = readQuestions mylines
        ans = readAnswers mylines
        codes = readCodeBlocks mylines
        qs = buildQuiz questions ans codes
        correct = parseCorrectAnswers (lines ansTxt)
        expls = parseExplanations (lines ansTxt)
    m <- get
    let newModel =
          initialModel
            { questionsList = qs
            , correctAnswers = correct
            , explanations = expls
            , appPage = QuizPage
            , availableChapters = availableChapters m
            , currentChapter = Just n
            }
    put newModel
    -- fetch image manifest; on error just deliver an empty list
    getText
      (ms $ "/resources/chapter" <> show n <> "/images.txt")
      []
      (\resp -> GotImageList (fromMisoString (body resp)))
      ((\_ -> GotImageList "") :: Response MisoString -> Action)
    io $ do
      result <- (getLocalStorage (storageKey n) :: IO (Either MisoString String))
      return $ case result of
        Right raw -> LoadSavedAnswers raw
        Left _ -> LoadSavedAnswers ""
  GotImageList txt ->
    modify $ \m -> m{chapterImages = parseImageList txt}
  FetchFailed err ->
    modify $ \m -> m{loadingError = Just (ms err)}
  LoadSavedAnswers raw ->
    modify $ \m -> m{selectedAnswers = deserializeAnswers raw}
  NextQuestion ->
    modify $ \m ->
      m
        { currentQuestion =
            min (length (questionsList m)) (currentQuestion m + 1)
        }
  PrevQuestion ->
    modify $ \m ->
      m
        { currentQuestion =
            max 0 (currentQuestion m - 1)
        }
  ToggleAnswer opt -> do
    m <- get
    case currentQuestionData m of
      Nothing -> pure ()
      Just q ->
        let qid = questionId q
            cur = M.findWithDefault S.empty qid (selectedAnswers m)
            newSet
              | isMultiple q =
                  if S.member opt cur
                    then S.delete opt cur
                    else S.insert opt cur
              | otherwise = S.singleton opt
            newAnswers = M.insert qid newSet (selectedAnswers m)
         in do
              put m{selectedAnswers = newAnswers}
              case currentChapter m of
                Just n -> io_ $ setLocalStorage (storageKey n) (serializeAnswers newAnswers :: String)
                Nothing -> pure ()
  Evaluate -> modify $ \m -> m{appPage = ResultsPage}
  BackToQuiz -> modify $ \m -> m{appPage = QuizPage}
  BackToChapterSelection ->
    modify $ \m ->
      initialModel
        { availableChapters = availableChapters m
        , appPage = ChapterSelectionPage
        }
  ResetAnswers -> do
    m <- get
    put m{selectedAnswers = M.empty}
    case currentChapter m of
      Just n -> io_ $ removeLocalStorage (storageKey n)
      Nothing -> pure ()
  NoOp -> pure ()

-- ── Helpers ────────────────────────────────────────────────────────────────

currentQuestionData :: Model -> Maybe Question
currentQuestionData m
  | null (questionsList m) = Nothing
  | currentQuestion m >= length (questionsList m) = Nothing
  | otherwise = Just (questionsList m Prelude.!! currentQuestion m)

isOnLastPage :: Model -> Bool
isOnLastPage m = currentQuestion m >= length (questionsList m)

questionIsCorrect :: Model -> Question -> Bool
questionIsCorrect m q =
  let sel = M.findWithDefault S.empty (questionId q) (selectedAnswers m)
      corr = M.findWithDefault S.empty (questionId q) (correctAnswers m)
   in not (S.null corr) && sel == corr

-- ── Top-level view dispatcher ──────────────────────────────────────────────

viewModel :: Model -> View Model Action
viewModel m
  | Just err <- loadingError m =
      div_
        [CSS.style_ containerSt]
        [ h1_ [CSS.style_ headingSt] ["Error"]
        , p_ [CSS.style_ ["color" =: "#dc2626"]] [text err]
        , button_ [onClick BackToChapterSelection, CSS.style_ btnSt] ["← Back to Chapters"]
        ]
  | otherwise = case appPage m of
      ChapterSelectionPage -> viewChapterSelection m
      LoadingPage ->
        div_ [CSS.style_ containerSt] [p_ [] ["Loading…"]]
      ResultsPage -> viewResults m
      QuizPage
        | null (questionsList m) ->
            div_ [CSS.style_ containerSt] [p_ [] ["Loading…"]]
        | isOnLastPage m -> viewLastPage m
        | otherwise -> case currentQuestionData m of
            Nothing -> div_ [CSS.style_ containerSt] [p_ [] ["Loading…"]]
            Just q -> viewQuizPage m q

-- ── Chapter selection page ──────────────────────────────────────────────────

viewChapterSelection :: Model -> View Model Action
viewChapterSelection m =
  div_
    [CSS.style_ containerSt]
    [ h1_ [CSS.style_ headingSt] ["Java Certification Quiz"]
    , p_ [CSS.style_ countSt] ["Choose a chapter to start the quiz"]
    , if null (availableChapters m)
        then p_ [] ["Loading chapters…"]
        else div_ [CSS.style_ chapterGridSt] (map renderChapterCard (availableChapters m))
    ]

renderChapterCard :: ChapterInfo -> View Model Action
renderChapterCard ci =
  div_
    [ CSS.style_ chapterCardSt
    , onClick (SelectChapter (chapterNum ci))
    ]
    [ div_ [CSS.style_ chapterNumSt] [text $ ms $ "Chapter " <> show (chapterNum ci)]
    , div_ [CSS.style_ chapterNameSt] [text (chapterName ci)]
    ]

-- ── Quiz page ──────────────────────────────────────────────────────────────

-- | Render a quiz image that auto-hides if the file is missing.
-- The JS in index.html captures 'error' events on elements with data-quiz-img.
-- keyProp ensures each unique URL is a fresh DOM element, preventing stale
-- display:none (set by the error handler) from persisting across navigation.
quizImg :: String -> [CSS.Style] -> View Model Action
quizImg url st =
  img_
    [ src_ (ms url)
    , textProp "data-quiz-img" "1"
    , keyProp (ms url)
    , CSS.style_ st
    ]

-- | Image URL for a question (e.g. /resources/chapter3/24.png)
questionImgUrl :: Int -> Int -> String
questionImgUrl chap qid =
  "/resources/chapter" <> show chap <> "/" <> show qid <> ".png"

-- | Image URL for an answer option (e.g. /resources/chapter3/142_D.png)
optionImgUrl :: Int -> Int -> Char -> String
optionImgUrl chap qid opt =
  "/resources/chapter" <> show chap <> "/" <> show qid <> "_" <> [opt] <> ".png"

viewQuizPage :: Model -> Question -> View Model Action
viewQuizPage m q =
  div_
    [CSS.style_ containerSt]
    [ h1_ [CSS.style_ headingSt] ["Quiz"]
    , p_
        [CSS.style_ countSt]
        [ text $
            ms $
              "Question "
                <> show (questionId q)
                <> " of "
                <> show (length (questionsList m))
        ]
    , div_ [CSS.style_ questionSt] [text (questionText q)]
    , if questionCode q == ""
        then text ""
        else pre_ [CSS.style_ codeSt] [text (questionCode q)]
    , let imgFile = show (questionId q) <> ".png"
      in case currentChapter m of
           Just n | S.member imgFile (chapterImages m) ->
             quizImg (questionImgUrl n (questionId q)) questionImgSt
           _ -> text ""
    , if isMultiple q
        then p_ [CSS.style_ hintSt] ["(Select all that apply)"]
        else text ""
    , div_ [CSS.style_ tableSt] (map (renderOption m q) (questionOptions q))
    , div_
        [CSS.style_ navSt]
        [ button_ [onClick PrevQuestion, CSS.style_ btnSt] ["← Previous"]
        , button_ [onClick NextQuestion, CSS.style_ btnSt] ["Next →"]
        , button_ [onClick BackToChapterSelection, CSS.style_ chapterBtnSt] ["⊞ Chapters"]
        , button_ [onClick ResetAnswers, CSS.style_ resetBtnSt] ["↺ Reset"]
        , button_ [onClick Evaluate, CSS.style_ evalBtnSt] ["✓ Evaluate"]
        ]
    ]

-- ── Last page ──────────────────────────────────────────────────────────────

viewLastPage :: Model -> View Model Action
viewLastPage m =
  let total = length (questionsList m)
      answered = M.size (selectedAnswers m)
   in div_
        [CSS.style_ containerSt]
        [ h1_ [CSS.style_ headingSt] ["Quiz Complete"]
        , div_
            [CSS.style_ lastBoxSt]
            [ p_
                [CSS.style_ lastTextSt]
                [ text $
                    ms $
                      "You have answered "
                        <> show answered
                        <> " out of "
                        <> show total
                        <> " questions."
                ]
            , p_
                [CSS.style_ lastHintSt]
                ["Click \"Evaluate\" to see your score."]
            ]
        , div_
            [CSS.style_ navSt]
            [ button_ [onClick PrevQuestion, CSS.style_ btnSt] ["← Previous"]
            , button_ [onClick BackToChapterSelection, CSS.style_ chapterBtnSt] ["⊞ Chapters"]
            , button_ [onClick ResetAnswers, CSS.style_ resetBtnSt] ["↺ Reset"]
            , button_ [onClick Evaluate, CSS.style_ evalBtnSt] ["✓ Evaluate Answers"]
            ]
        ]

-- ── Results page ───────────────────────────────────────────────────────────

viewResults :: Model -> View Model Action
viewResults m =
  let qs = questionsList m
      total = length qs
      numCorr = length $ filter (questionIsCorrect m) qs
      pct = if total == 0 then 0 else (numCorr * 100) `div` total
   in div_
        [CSS.style_ containerSt]
        [ h1_ [CSS.style_ headingSt] ["Results"]
        , div_
            [CSS.style_ (scoreBannerSt pct)]
            [ p_
                [CSS.style_ scoreLabelSt]
                [text $ ms $ show numCorr <> " / " <> show total <> " correct"]
            , p_
                [CSS.style_ pctSt]
                [text $ ms $ show pct <> "%"]
            ]
        , div_ [CSS.style_ resultGridSt] (map (renderResultCell m) qs)
        , div_
            [CSS.style_ navSt]
            [ button_ [onClick BackToQuiz, CSS.style_ btnSt] ["← Back to Quiz"]
            , button_ [onClick BackToChapterSelection, CSS.style_ chapterBtnSt] ["⊞ Chapters"]
            ]
        ]

renderResultCell :: Model -> Question -> View Model Action
renderResultCell m q =
  let sel = M.findWithDefault S.empty (questionId q) (selectedAnswers m)
      corr = M.findWithDefault S.empty (questionId q) (correctAnswers m)
      expl = M.findWithDefault "" (questionId q) (explanations m)
      unanswered = S.null sel
      ok = questionIsCorrect m q
      cellSt
        | unanswered = resultCellUnansweredSt
        | ok = resultCellCorrectSt
        | otherwise = resultCellWrongSt
      mark :: String
      mark
        | unanswered = "–"
        | ok = "✓"
        | otherwise = "✗"
   in div_
        [CSS.style_ cellSt, textProp "title" (ms expl)]
        [ div_ [CSS.style_ cellNumSt] [text $ ms $ "Q" <> show (questionId q)]
        , div_ [CSS.style_ cellMarkSt] [text $ ms mark]
        , if not ok && not unanswered
            then
              div_
                [CSS.style_ cellDetailSt]
                [ text $ ms $ "You: " <> S.toList sel
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
  let sel = M.findWithDefault S.empty (questionId q) (selectedAnswers m)
      isChecked = S.member opt sel
      inType = if isMultiple q then "checkbox" else "radio"
      rName = ms ("q" <> show (questionId q))
      rowSt
        | isChecked = ("background" =: "#e8f4fd") : rowBaseSt
        | otherwise = rowBaseSt
      optImg =
        let imgFile = show (questionId q) <> "_" <> [opt] <> ".png"
        in case currentChapter m of
             Just n | S.member imgFile (chapterImages m) ->
               quizImg (optionImgUrl n (questionId q) opt) optionImgSt
             _ -> text ""
   in div_
        [CSS.style_ rowSt]
        [ div_
            [CSS.style_ inputCellSt]
            [input_ [type_ inType, name_ rName, checked_ isChecked, onClick (ToggleAnswer opt)]]
        , div_ [CSS.style_ letterCellSt] [text $ ms [opt]]
        , div_ [CSS.style_ textCellSt] [text labelText, optImg]
        ]

-- ── Styles ─────────────────────────────────────────────────────────────────

type St = [CSS.Style]

containerSt :: St
containerSt =
  [ "max-width" =: "860px"
  , "margin" =: "0 auto"
  , "padding" =: "24px"
  , "font-family" =: "system-ui, sans-serif"
  , "color" =: "#222"
  ]

headingSt :: St
headingSt = ["margin-bottom" =: "4px", "color" =: "#1a56db"]

countSt :: St
countSt = ["color" =: "#6b7280", "font-size" =: "0.9em", "margin" =: "0 0 12px"]

questionSt :: St
questionSt =
  [ "font-size" =: "1.05em"
  , "line-height" =: "1.6"
  , "margin-bottom" =: "16px"
  , "white-space" =: "pre-wrap"
  ]

hintSt :: St
hintSt = ["color" =: "#6b7280", "font-size" =: "0.85em", "margin" =: "-8px 0 10px"]

codeSt :: St
codeSt =
  [ "background" =: "#f6f8fa"
  , "border" =: "1px solid #d0d7de"
  , "border-radius" =: "6px"
  , "padding" =: "14px 16px"
  , "font-family" =: "ui-monospace, monospace"
  , "font-size" =: "0.88em"
  , "line-height" =: "1.5"
  , "overflow-x" =: "auto"
  , "margin-bottom" =: "16px"
  , "white-space" =: "pre"
  ]

tableSt :: St
tableSt =
  [ "display" =: "table"
  , "width" =: "100%"
  , "border-collapse" =: "separate"
  , "border-spacing" =: "0 4px"
  , "margin-bottom" =: "16px"
  ]

rowBaseSt :: St
rowBaseSt =
  ["display" =: "table-row", "border-radius" =: "4px", "cursor" =: "pointer"]

inputCellSt :: St
inputCellSt =
  [ "display" =: "table-cell"
  , "padding" =: "8px 6px"
  , "width" =: "32px"
  , "vertical-align" =: "middle"
  , "text-align" =: "center"
  ]

letterCellSt :: St
letterCellSt =
  [ "display" =: "table-cell"
  , "padding" =: "8px 6px"
  , "width" =: "36px"
  , "font-weight" =: "600"
  , "vertical-align" =: "middle"
  ]

textCellSt :: St
textCellSt =
  [ "display" =: "table-cell"
  , "padding" =: "8px 12px"
  , "vertical-align" =: "middle"
  , "line-height" =: "1.4"
  , "white-space" =: "pre-wrap"
  ]

navSt :: St
navSt =
  ["display" =: "flex", "gap" =: "12px", "margin-top" =: "20px", "flex-wrap" =: "wrap"]

btnSt :: St
btnSt =
  [ "padding" =: "10px 24px"
  , "font-size" =: "1em"
  , "cursor" =: "pointer"
  , "background" =: "#1a56db"
  , "color" =: "#fff"
  , "border" =: "none"
  , "border-radius" =: "6px"
  ]

evalBtnSt :: St
evalBtnSt =
  [ "padding" =: "10px 24px"
  , "font-size" =: "1em"
  , "cursor" =: "pointer"
  , "background" =: "#16a34a"
  , "color" =: "#fff"
  , "border" =: "none"
  , "border-radius" =: "6px"
  , "margin-left" =: "auto"
  ]

lastBoxSt :: St
lastBoxSt =
  [ "background" =: "#f0f9ff"
  , "border" =: "1px solid #bae6fd"
  , "border-radius" =: "8px"
  , "padding" =: "24px"
  , "margin" =: "24px 0"
  , "text-align" =: "center"
  ]

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
   in [ "background" =: bg
      , "border" =: ("1px solid " <> bord)
      , "border-radius" =: "8px"
      , "padding" =: "20px 24px"
      , "margin-bottom" =: "24px"
      , "text-align" =: "center"
      ]

scoreLabelSt :: St
scoreLabelSt = ["font-size" =: "1.3em", "font-weight" =: "600", "margin" =: "0 0 4px"]

pctSt :: St
pctSt = ["font-size" =: "3em", "font-weight" =: "700", "margin" =: "0"]

resultGridSt :: St
resultGridSt =
  [ "display" =: "grid"
  , "grid-template-columns" =: "repeat(auto-fill, minmax(110px, 1fr))"
  , "gap" =: "8px"
  , "margin-bottom" =: "24px"
  ]

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

chapterGridSt :: St
chapterGridSt =
  [ "display" =: "grid"
  , "grid-template-columns" =: "repeat(auto-fill, minmax(220px, 1fr))"
  , "gap" =: "16px"
  , "margin-top" =: "24px"
  ]

chapterCardSt :: St
chapterCardSt =
  [ "background" =: "#f0f7ff"
  , "border" =: "2px solid #1a56db"
  , "border-radius" =: "10px"
  , "padding" =: "20px 24px"
  , "cursor" =: "pointer"
  , "transition" =: "background 0.15s"
  ]

chapterNumSt :: St
chapterNumSt =
  [ "font-size" =: "0.85em"
  , "font-weight" =: "600"
  , "color" =: "#1a56db"
  , "margin-bottom" =: "6px"
  ]

chapterNameSt :: St
chapterNameSt =
  [ "font-size" =: "1.05em"
  , "font-weight" =: "700"
  , "color" =: "#222"
  ]

chapterBtnSt :: St
chapterBtnSt =
  [ "padding" =: "10px 24px"
  , "font-size" =: "1em"
  , "cursor" =: "pointer"
  , "background" =: "#6b7280"
  , "color" =: "#fff"
  , "border" =: "none"
  , "border-radius" =: "6px"
  ]

resetBtnSt :: St
resetBtnSt =
  [ "padding" =: "10px 24px"
  , "font-size" =: "1em"
  , "cursor" =: "pointer"
  , "background" =: "#dc2626"
  , "color" =: "#fff"
  , "border" =: "none"
  , "border-radius" =: "6px"
  ]

questionImgSt :: St
questionImgSt =
  [ "max-width" =: "100%"
  , "display" =: "block"
  , "margin" =: "0 0 16px"
  , "border-radius" =: "6px"
  , "border" =: "1px solid #d0d7de"
  ]

optionImgSt :: St
optionImgSt =
  [ "max-width" =: "340px"
  , "display" =: "block"
  , "margin" =: "6px 0 2px"
  , "border-radius" =: "4px"
  , "border" =: "1px solid #d0d7de"
  ]

