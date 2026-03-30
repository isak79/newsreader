module Main where

import Text.Feed.Import

type Title = String
type PubDate = String
type Source = String
type Entry = (Title, Source, PubDate)

main :: IO ()
main = do
  contents <- parseFeedFromFile "testdata/vgfeed"
  print contents
