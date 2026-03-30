module Main where

import ParseFeed (parseFeed)
import Brick

ui :: String -> Widget ()
ui x = str x

main :: IO ()
main = do
  entries <- parseFeed
  simpleMain $ ui $ show entries
