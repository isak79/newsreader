module ParseFeed(parseFeed, Entry(..), fallbackEntry) where

import Text.Feed.Query
import Text.Feed.Types
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Maybe (mapMaybe)
import FetchFeed
import Control.Monad (join)
import Data.Text (Text)

type URL = T.Text

data Entry = Entry {  title       :: T.Text
                    , source      :: T.Text
                    , pubTime     :: Maybe UTCTime
                    , description :: Maybe T.Text 
                    , isRead      :: Bool
                    , article     :: Maybe Text }
                    deriving (Show, Eq)

-- | Takes a URL, returns parsed entry
parseFeed :: URL -> IO [Entry]
parseFeed url = do
  feed <- fetchFeed url
  entries feed

-- | Convert a feed Item into an Entry
toEntry :: Item -> Maybe Entry
toEntry i = do
  title0      <- getItemTitle i
  source0     <- getItemLink i
  let description = getItemDescription i
      pubTime     = join (getItemPublishDate i)
      title  = T.strip title0
      source = cleanUrl source0
  pure Entry { title, source, pubTime, description, isRead = False, article = Nothing }

cleanUrl :: T.Text -> T.Text
cleanUrl t = case T.words t of
  (u:_) -> u
  []    -> T.empty

-- | Backup entry, displayed in empty mailboxes
fallbackEntry :: Entry
fallbackEntry = Entry { title = T.pack "Nothing to show"
                      , source = T.pack "No url"
                      , pubTime = Nothing
                      , description = Nothing
                      , isRead = True 
                      , article = Nothing }

entries :: Applicative f => Maybe Feed -> f [Entry]
entries feed = do
  case feed of
    Nothing   -> pure []
    Just fee  -> pure (mapMaybe toEntry (feedItems fee))
