module Main where

import ParseFeed (parseFeed)
import Brick

ui :: String -> Widget ()
ui = str

main :: IO ()
main = do
  entries <- parseFeed
  simpleMain $ ui $ show entries
