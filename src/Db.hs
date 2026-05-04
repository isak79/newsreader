{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}

module Db(fetchEntries, fetchMailboxes, refreshAll, readEntry, getFeeds, URL, addFeedToMailbox, initializeTables, insertMailbox, updateFeedUrl, updateMailboxName, moveFeed, deleteFeed, deleteMailbox) where

import Database.Selda
import Database.Selda.SQLite
import ParseFeed (parseFeed, Entry(..), parseFeed)
import ParseNews

type URL         = Text

data DbEntry = DbEntry 
  {
    eID           :: ID DbEntry   
  , dbTitle       :: Text
  , dbSource      :: Text
  , dbPubTime     :: Maybe UTCTime
  , dbDescription :: Maybe Text 
  , dedup         :: Text
  , dbIsRead      :: Bool
  , feedID        :: ID DbFeeds
  , dbArticle     :: Maybe Text
  } deriving (Show, Eq, Generic)

instance SqlRow DbEntry

dbEntries :: Table DbEntry
dbEntries = table "entries" [ #eID   :- autoPrimary
                            , #dedup :- unique ]

-- | Delete a feed with the url matching the input
deleteFeed :: (MonadIO m, MonadMask m) => Text -> m ()
deleteFeed feedUrl = withSQLite "newsreader.db" $ do
  feedID <- query $ do
    feed <- select dbFeeds
    restrict (feed ! #url .== literal feedUrl)
    pure (feed ! #fID)
  deleteFrom_ dbFeeds (\r -> r ! #fID .== literal (head feedID))
  deleteFrom_ dbEntries (\r -> r ! #feedID .== literal (head feedID))

-- | Deletes the mailbox with name given as input
deleteMailbox :: (MonadMask m, MonadIO m) => Text -> m ()
deleteMailbox mbName = withSQLite "newsreader.db" $ do
  deleteFrom_ dbMailboxes (\r -> r ! #name .== literal mbName)

-- | Convert an Entry to a DbEntry
toDbEntry  ::  ID DbFeeds -> Entry -> DbEntry
toDbEntry feedid ent = DbEntry {
    eID           = def
  , dbTitle       = title'
  , dbSource      = source'
  , dbPubTime     = pubTime ent
  , dbDescription = description ent
  , dedup         = dedupString
  , dbIsRead      = isRead ent
  , feedID        = feedid
  , dbArticle     = article ent
  }
  where 
    title'  = title ent
    source' = source ent
    dedupString = title' <> "\x1f" <> source'

-- | Convert a DbEntry to an Entry
fromDbEntry :: DbEntry -> Entry
fromDbEntry dbEnt = Entry {
    title       = dbTitle dbEnt
  , source      = dbSource dbEnt
  , pubTime     = dbPubTime dbEnt
  , description = dbDescription dbEnt
  , isRead      = dbIsRead dbEnt
  , article     = dbArticle dbEnt 
  }


data DbMailbox = DbMailbox {
    mID           :: ID DbMailbox 
  , name          :: Text
  } deriving (Eq, Show, Generic)

instance SqlRow DbMailbox

dbMailboxes :: Table DbMailbox 
dbMailboxes = table "mailboxes" [ #mID  :- autoPrimary
                                , #name :- unique ]


data DbFeeds = DbFeeds {
    fID       :: ID DbFeeds
  , mailboxID :: ID DbMailbox
  , url       :: URL
  } deriving (Eq, Show, Generic)

instance SqlRow DbFeeds

-- | Move a feed with `url` to the mailbox with the name `newMb`
moveFeed :: (MonadMask m, MonadIO m) => Text -> Text -> m ()
moveFeed url newMb = withSQLite "newsreader.db" $ do
  -- feed <- select dbFeeds
  -- restrict (feed ! #url .== literal url)
  mb <- query $ do
    mailbox <- select dbMailboxes 
    restrict (mailbox ! #name .== literal newMb)
    pure (mailbox ! #mID) 
  case mb of
    [m] -> update_ dbFeeds (\row -> row ! #url .== literal url) (\row -> row `with` [#mailboxID := literal m ])
    _   -> pure ()


dbFeeds :: Table DbFeeds
dbFeeds = table "feeds" [ #fID :- autoPrimary
                        , #url :- unique ]

-- | Fetch every feed from the database
getFeeds :: (MonadMask m, MonadIO m) => m [(URL, Text)]
getFeeds = withSQLite "newsreader.db" $ do
  rows <- query $ do
    feed <- select dbFeeds
    mailbox <- select dbMailboxes
    restrict (feed ! #mailboxID .== mailbox ! #mID)
    pure (feed ! #url :*: mailbox ! #name)
  pure [(u, mbName) | u :*: mbName <- rows ]

-- | Insert a new mailbox with the name given as input
insertMailbox :: (MonadIO m, MonadMask m) => Text -> m ()
insertMailbox mailboxName = withSQLite "newsreader.db" $ do
  insert_ dbMailboxes [DbMailbox def mailboxName]

-- | Takes in a url and a mailbox name, add the feed to mailbox, create mailbox if it doesn't exist
addFeedToMailbox :: (MonadIO m, MonadMask m) => URL -> Text -> m ()
addFeedToMailbox url mailboxName = withSQLite "newsreader.db" $ do
  mid <- mailboxIDfromName mailboxName
  mailboxID <- case mid of
    (x:_) -> pure x
    []    -> insertWithPK dbMailboxes [DbMailbox def mailboxName]
  insert_ dbFeeds [DbFeeds def mailboxID url]

-- | Fetch entries from every feed currently subscribed to, insert the entries into database, if they are not already there
refreshAll :: IO ()
refreshAll = withSQLite "newsreader.db" $ do
  urlFids <- queryUrlsAndFeedIDs 
  dbEntries0 <- concat <$> traverse (\(url, fid) -> liftIO $ map (toDbEntry fid) <$> parseFeed url) urlFids 
  let dbEntries1 = map (:[]) dbEntries0 
  _ <- traverse (tryInsert dbEntries) dbEntries1
  pure ()
    where  
      queryUrlsAndFeedIDs :: MonadSelda m => m [(URL, ID DbFeeds)]
      queryUrlsAndFeedIDs = do
        fs <- query $ select dbFeeds
        pure [ (url f, fID f) | f <- fs ]
      
-- | Return a mailbox id from given mailbox name
mailboxIDfromName :: MonadSelda m => Text -> m [ID DbMailbox]
mailboxIDfromName mailboxName = query $ do
    mailbox <- select dbMailboxes
    restrict (mailbox ! #name .== literal mailboxName)
    pure (mailbox ! #mID)
    
-- | Create database tables if they don't already exist
initializeTables :: IO ()
initializeTables = withSQLite "newsreader.db" $ do
  tryCreateTable dbEntries
  tryCreateTable dbMailboxes
  tryCreateTable dbFeeds 

-- | Query all mailboxes from the database
fetchMailboxes :: (MonadMask m, MonadIO m) => m [Text]
fetchMailboxes = withSQLite "newsreader.db" $ do
  mb <- query $ select dbMailboxes 
  pure (map name mb)

-- | Query all the entries from the database
fetchEntries :: (MonadMask m, MonadIO m) => Text -> m [Entry]
fetchEntries mailboxName = withSQLite "newsreader.db" $ do
  result <- query $ do
    mailbox <- select dbMailboxes 
    restrict (mailbox ! #name .== literal mailboxName)
    dbFeed <- select dbFeeds  
    restrict (dbFeed ! #mailboxID .== mailbox ! #mID)
    dbEntry <- select dbEntries 
    restrict (dbEntry ! #feedID .== dbFeed ! #fID)
    pure dbEntry
  pure $ map fromDbEntry result

-- | Replace a feed url with a new one
updateFeedUrl :: URL -> URL -> IO ()
updateFeedUrl oldUrl newUrl = withSQLite "newsreader.db" $ do
  update_ dbFeeds (\row -> row ! #url .== literal oldUrl) (\row -> row `with` [#url := literal newUrl])

-- | Replace a mailbox name with a new one
updateMailboxName :: Text -> Text -> IO ()
updateMailboxName oldName newName = withSQLite "newsreader.db" $ do
  update_ dbMailboxes (\row -> row ! #name .== literal oldName) (\row -> row `with` [#name := literal newName])
  
-- | Mark an entry as read
readEntry :: Bool -> Entry -> IO ()
readEntry b ent = withSQLite "newsreader.db" $ do
  case b of 
    True  -> update_ dbEntries (\r -> r ! #dbSource .== literal (source ent)) (\r -> r `with` [#dbIsRead := true])
    False -> update_ dbEntries (\r -> r ! #dbSource .== literal (source ent)) (\r -> r `with` [#dbIsRead := false])
