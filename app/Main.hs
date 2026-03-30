module Main where

import Text.Feed.Import
import Text.Feed.Query

type Title = String
type PubDate = String
type Source = String
type Entry = (Title, Source, PubDate)

main :: IO ()
main = do
  print "hei"

contents = do
  cont <- parseFeedFromFile "testdata/vgfeed"
  case cont of
    Nothing
      -> pure []
    (Just feed)
      ->  do
        items <- feedItems feed
        pure items
    

