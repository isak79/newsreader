module Main where

import ParseFeed (parseFeed, Entry)
import Brick

ui :: String -> Widget ()
ui = str

main :: IO ()
main = do
  tuiState <- buildState
  print tuiState 

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)


buildState = do
  entries <- parseFeed
  pure TuiState { entries }

newtype TuiState = TuiState { entries :: [Entry] }
  deriving Show

-- drawTui :: TuiState -> Widget String
drawTui ts = [vBox $ map drawEntry $ entries ts]

drawEntry :: Entry -> Widget n
drawEntry e = str $ show e
