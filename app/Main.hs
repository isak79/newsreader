module Main where

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types

type Title = String
type PubDate = String
type Source = String
type Entry = (Title, Source, PubDate)

main :: IO ()
main = do
  print "hei"

toEntry :: [Item] -> [Entry]
toEntry i = (getItemTitle i, getItemLink i, getItemPublishDate i)

contents = do
  cont <- parseFeedFromFile "testdata/vgfeed"
  case cont of
    Nothing   -> pure []
    Just feed ->  pure (map toEntry (feedItems feed))
