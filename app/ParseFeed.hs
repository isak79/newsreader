module ParseFeed(parseFeed, Entry(..)) where

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Maybe (catMaybes)

data Entry = Entry {  title :: T.Text
                    , source :: T.Text
                    , pubTime :: Maybe UTCTime } 
                    deriving Show

parseFeed :: IO [Entry]
parseFeed = do
  feed <- parseFeedFromFile "testdata/vgfeed"
  entries feed

toEntry :: Item -> Maybe Entry
toEntry i = do
  title0   <- getItemTitle i
  source0  <- getItemLink i
  pubTime <- getItemPublishDate i
  let title  = T.strip title0
      source = cleanUrl source0
  pure Entry {title, source, pubTime}

cleanUrl :: T.Text -> T.Text
cleanUrl t = case T.words t of
  (u:_) -> u
  []    -> T.empty 

entries :: Applicative f => Maybe Feed -> f [Entry]
entries feed = do
  case feed of
    Nothing   -> pure []
    Just fee  -> pure (catMaybes (map toEntry (feedItems fee)))
