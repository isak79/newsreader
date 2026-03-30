module Main where

import ParseFeed (parseFeed)
import Brick

ui :: String -> Widget ()
ui x = str x

main :: IO ()
main = do
  f <- parseFeed
  simpleMain $ ui f
