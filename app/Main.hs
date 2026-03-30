module Main where

import ParseFeed (parseFeed)
import Brick

ui :: String -> Widget ()
ui x = str x

main :: IO ()
main = do
  f <- parseFeed
  -- case f of
  -- Nothing   -> error "something went wrong"
  -- Just feed -> simpleMain $ ui feed
  print f
