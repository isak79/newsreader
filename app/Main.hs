module Main where

import ParseFeed (parseFeed, Entry)
import Brick
import Data.Text (Text)

ui :: String -> Widget ()
ui x = str x

main :: IO ()
main = do
  entries <- parseFeed
  simpleMain $ ui $ show entries
