module Main where

import ParseFeed (parseFeed)

main :: IO ()
main = do
  f <- parseFeed
  print f
