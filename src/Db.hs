{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}

module Db where

import Database.Selda
import Database.Selda.SQLite
import ParseFeed (Entry(..), parseFeed)

type MailboxName = Text
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


toDbEntry  :: Entry -> DbEntry
toDbEntry ent = DbEntry {
    eID           = def
  , dbTitle       = title'
  , dbSource      = source'
  , dbPubTime     = pubTime ent
  , dbDescription = description ent
  , dedup         = dedupString
  , dbIsRead      = isRead ent
  , feedID        = def
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

feeds :: Table DbFeeds
feeds = table "feeds" [ #fID :- autoPrimary
                      , #url :- unique ]



addFeedToMailbox :: (MonadIO m, MonadMask m) => URL -> Text -> m ()
addFeedToMailbox url mailboxName = withSQLite "newsreader.sqlite" $ do
  mid <- query $ do
    mailbox <- select dbMailboxes
    restrict (mailbox ! #name .== literal mailboxName)
    pure (mailbox ! #mID)

  mailboxID <- case mid of
    (x:_) -> pure x
    []    -> insertWithPK dbMailboxes [DbMailbox def mailboxName]

  insert_ feeds [DbFeeds def mailboxID url]

    

initializeTables :: IO ()
initializeTables = withSQLite "newsreader.sqlite" $ do
  tryCreateTable dbEntries
  tryCreateTable dbMailboxes
  tryCreateTable feeds 

