module ParseFeed(parseFeed) where

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Data.Text (Text)
import Data.Time (UTCTime)

type Title = Text
type Source = Text
type PubTime = Maybe UTCTime
type Entry = (Title, Source, PubTime)

parseFeed :: IO [Maybe Entry]
parseFeed = do
  feed <- parseFeedFromFile "testdata/vgfeed"
  entries feed

toEntry :: Item -> Maybe Entry
toEntry i = do
  title <- getItemTitle i
  source  <- getItemLink i
  pubTime <- getItemPublishDate i
  pure (title, source, pubTime)

entries :: Applicative f => Maybe Feed -> f [Maybe Entry]
entries feed = do
  case feed of
    Nothing   -> pure []
    Just fee ->  pure (map toEntry (feedItems fee))
