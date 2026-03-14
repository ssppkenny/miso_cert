{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Miso
import Miso.String (MisoString, ms)
-- import qualified Data.Text as T
-- import Control.Monad.IO.Class (liftIO)
import System.IO ()
import Language.Javascript.JSaddle.Warp (run)

-- | The application state: list of words and current word index
data Model = Model
  { wordsList   :: [MisoString]  -- all words from the file
  , currentWord :: Int           -- index of the current word
  } deriving (Show, Eq)

-- | Actions that can be triggered in the app
data Action
  = NextWord
  | PrevWord
  | SetWords [MisoString]        -- set the words after loading file
  deriving (Show, Eq)

-- | Entry point for JSaddle Warp
main :: IO ()
main = do
  -- Load words.txt at startup
  content <- readFile "words.txt"
  let ws = map ms $ lines content
  run 3000 $ startApp App
    { initialAction = SetWords ws
    , model  = Model [] 0
    , update = updateModel
    , view   = viewModel
    , events = defaultEvents
    , subs   = []
    , mountPoint = Nothing
    , logLevel = Off
    }

-- | Update function
updateModel :: Action -> Model -> Effect Action Model
updateModel = \case
  NextWord -> \m ->
    noEff $ m { currentWord = min (length (wordsList m) - 1) (currentWord m + 1) }

  PrevWord -> \m ->
    noEff $ m { currentWord = max 0 (currentWord m - 1) }

  SetWords ws -> \_ -> noEff $ Model ws 0
-- | View function
viewModel :: Model -> View Action
viewModel m =
  div_ []
    [ h1_ [] ["📚 Word Viewer"]
    , div_ []
        [ text $ if null (wordsList m)
                   then "Loading..."
                   else wordsList m !! currentWord m
        ]
    , div_ []
        [ button_ [ onClick PrevWord ] ["Previous"]
        , button_ [ onClick NextWord ] ["Next"]
        ]
    ]
