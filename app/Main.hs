module Main where

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Data.Text (Text)

type Title = Text
type PubDate = String
type Source = Text
type Entry = (Title, Source)

main :: IO ()
main = do
  print "hei"

-- toEntry :: Item -> Maybe Entry
toEntry i = (getItemTitle i, getItemLink i)

contents = do
  cont <- parseFeedFromFile "testdata/vgfeed"
  case cont of
    Nothing   -> pure []
    Just feed ->  pure (map toEntry (feedItems feed))
