module Main where

import ParseFeed (parseFeed, Entry)
import Brick

ui :: String -> Widget ()
ui = str

main :: IO ()
main = do
  entries <- parseFeed
  simpleMain $ ui $ show entries

newtype TuiState = TuiState { entries :: [Entry] }
  deriving Show

drawTui :: TuiState -> Widget String
drawTui ts = [vBox $ map drawEntry $ entries]

drawEntry :: Entry -> Widget n
drawEntry e = str (show e.title ++ "\n" ++ show e.source ++ "\n")
