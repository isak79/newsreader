module ParseFeed(parseFeed, Entry(..)) where

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Maybe (catMaybes)

data Entry = Entry {  title :: Text
                    , source :: Text
                    , pubTime :: Maybe UTCTime } 
                    deriving Show

parseFeed :: IO [Entry]
parseFeed = do
  feed <- parseFeedFromFile "testdata/vgfeed"
  entries feed

toEntry :: Item -> Maybe Entry
toEntry i = do
  title   <- getItemTitle i
  source  <- getItemLink i
  pubTime <- getItemPublishDate i
  pure Entry {title, source, pubTime}

entries :: Applicative f => Maybe Feed -> f [Entry]
entries feed = do
  case feed of
    Nothing   -> pure []
    Just fee  -> pure (catMaybes (map toEntry (feedItems fee)))
