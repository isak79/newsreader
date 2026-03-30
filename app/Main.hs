module Main where

import Brick

ui :: String -> Widget ()
ui x = str x

main :: IO ()
main = do
  simpleMain $ ui "Hello, World!"
