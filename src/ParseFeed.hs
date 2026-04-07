module ParseFeed(parseFeed, Entry(..)) where

import Text.Feed.Query
import Text.Feed.Types
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Maybe (mapMaybe)
import FetchFeed

data Entry = Entry {  title :: T.Text
                    , source :: T.Text
                    , pubTime :: Maybe UTCTime
                    , description :: Maybe T.Text }
                    deriving Show

parseFeed :: IO [Entry]
parseFeed = do
  feed <- fetchFeed 
  entries feed

toEntry :: Item -> Maybe Entry
toEntry i = do
  title0      <- getItemTitle i
  source0     <- getItemLink i
  let description = getItemDescription i
  pubTime     <- getItemPublishDate i
  let title  = T.strip title0
      source = cleanUrl source0
  pure Entry { title, source, pubTime, description }

cleanUrl :: T.Text -> T.Text
cleanUrl t = case T.words t of
  (u:_) -> u
  []    -> T.empty

entries :: Applicative f => Maybe Feed -> f [Entry]
entries feed = do
  case feed of
    Nothing   -> pure []
    Just fee  -> pure (mapMaybe toEntry (feedItems fee))
