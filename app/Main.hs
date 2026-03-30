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
  feed <- parseFeedFromFile "testdata/vgfeed"
  entri <- entries feed
  print entri

toEntry :: Item -> Maybe Entry
toEntry i = do
  title <- getItemTitle i
  source  <- getItemLink i
  pure (title, source)

entries :: Applicative f => Maybe Feed -> f [Maybe Entry]
entries feed = do
  case feed of
    Nothing   -> pure []
    Just fee ->  pure (map toEntry (feedItems fee))
