{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}

module Db(fetchEntries, fetchMailboxes, refreshAll, readEntry, getFeeds, URL, addFeedToMailbox, initializeTables) where

import Database.Selda
import Database.Selda.SQLite
import ParseFeed (parseFeed, Entry(..), parseFeed)
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
  } deriving (Show, Eq, Generic)

instance SqlRow DbEntry

dbEntries :: Table DbEntry
dbEntries = table "entries" [ #eID   :- autoPrimary
                            , #dedup :- unique]


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
  }
  where 
    title'  = title ent
    source' = source ent
    dedupString = title' <> "\x1f" <> source'

fromDbEntry :: DbEntry -> Entry
fromDbEntry dbEnt = Entry {
    title       = dbTitle dbEnt
  , source      = dbSource dbEnt
  , pubTime     = dbPubTime dbEnt
  , description = dbDescription dbEnt
  , isRead      = dbIsRead dbEnt
  }


data DbMailbox = DbMailbox {
    mID           :: ID DbMailbox 
  , name          :: Text
  } deriving (Eq, Show, Generic)

instance SqlRow DbMailbox

dbMailboxes :: Table DbMailbox 
dbMailboxes = table "mailboxes" [#mID :- autoPrimary]


data DbFeeds = DbFeeds {
    fID :: ID DbFeeds
  , mailboxID :: ID DbMailbox
  , url :: URL
  } deriving (Eq, Show, Generic)

instance SqlRow DbFeeds

dbFeeds :: Table DbFeeds
dbFeeds = table "feeds" [ #fID :- autoPrimary
                        , #url :- unique ]

getFeeds :: (MonadMask m, MonadIO m) => m [(URL, Text)]
getFeeds = withSQLite "newsreader.db" $ do
  rows <- query $ do
    feed <- select dbFeeds
    mailbox <- select dbMailboxes
    restrict (feed ! #mailboxID .== mailbox ! #mID)
    pure (feed ! #url :*: mailbox ! #name)
  pure [ (u, mbName) | u :*: mbName <- rows ]


addFeedToMailbox :: (MonadIO m, MonadMask m) => URL -> Text -> m ()
addFeedToMailbox url mailboxName = withSQLite "newsreader.db" $ do
  mid <- mailboxIDfromName mailboxName
  mailboxID <- case mid of
    (x:_) -> pure x
    []    -> insertWithPK dbMailboxes [DbMailbox def mailboxName]

  insert_ dbFeeds [DbFeeds def mailboxID url]

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
      

mailboxIDfromName :: MonadSelda m => Text -> m [ID DbMailbox]
mailboxIDfromName mailboxName = query $ do
    mailbox <- select dbMailboxes
    restrict (mailbox ! #name .== literal mailboxName)
    pure (mailbox ! #mID)
    

initializeTables :: IO ()
initializeTables = withSQLite "newsreader.db" $ do
  tryCreateTable dbEntries
  tryCreateTable dbMailboxes
  tryCreateTable dbFeeds 

fetchMailboxes :: (MonadMask m, MonadIO m) => m [Text]
fetchMailboxes = withSQLite "newsreader.db" $ do
  mb <- query $ select dbMailboxes 
  pure (map name mb)


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


readEntry :: Bool -> Entry -> IO ()
readEntry b ent = withSQLite "newsreader.db" $ do
  case b of 
    True  -> update_ dbEntries (\row -> row ! #dbSource .== literal (source ent)) (\row -> row `with` [#dbIsRead := true])
    False -> update_ dbEntries (\row -> row ! #dbSource .== literal (source ent)) (\row -> row `with` [#dbIsRead := false])
