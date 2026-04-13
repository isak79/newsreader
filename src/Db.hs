{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}

module Db where

import Database.Selda
import Database.Selda.SQLite
import ParseFeed (Entry(..))

data DbEntry = DbEntry 
  {
    eID           :: ID DbEntry   
  , dbTitle       :: Text
  , dbSource      :: Text
  , dbPubTime     :: Maybe UTCTime
  , dbDescription :: Maybe Text 
  , dedup        :: Text
  } deriving (Show, Eq, Generic)

instance SqlRow DbEntry

entries :: Table DbEntry
entries = table "entries" [ #eID   :- autoPrimary
                          , #dedup :- unique]

data DbMailbox = DbMailbox {
    mID           :: ID DbMailbox 
  , name          :: Text
  , unreadEntries :: Int
  } deriving (Eq, Show, Generic)

instance SqlRow DbMailbox

data DbFeeds = DbFeeds {
    fID :: ID DbFeeds
  , url :: Text
  } deriving (Eq, Show, Generic)

instance SqlRow DbFeeds

feeds :: Table DbFeeds
feeds = table "feeds" [ #fID :- autoPrimary ]

data DbMailboxFeed = DbMailboxFeed {
    mfID       :: ID DbMailboxFeed
  , feedID     :: ID DbFeeds
  , mailboxID' :: ID DbMailbox
  } deriving (Eq, Show, Generic)

instance SqlRow DbMailboxFeed

mailboxFeeds :: Table DbMailboxFeed
mailboxFeeds = table "mailboxFeeds" [ #mfID :- autoPrimary ]

mailboxes :: Table DbMailbox 
mailboxes = table "mailboxes" [#mID :- autoPrimary]

data DbMailboxEntry = DbMailboxEntry {
    meID      :: ID DbMailboxEntry
  , entryID   :: ID DbEntry
  , mailboxID :: ID DbMailbox
  , isRead    :: Bool
  , addedAt   :: UTCTime
  } deriving (Show, Eq, Generic)

instance SqlRow DbMailboxEntry 
  
mailboxEntries :: Table DbMailboxEntry
mailboxEntries = table "mailboxEntries" [#meID :- autoPrimary]

type MailboxName = Text

toDbEntry  :: Entry -> DbEntry
toDbEntry ent = DbEntry {
    eID           = def
  , dbTitle       = title'
  , dbSource      = source'
  , dbPubTime     = pubTime ent
  , dbDescription = description ent
  , dedup         = dedupString
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
  }



initializeTables :: IO ()
initializeTables = withSQLite "newsreader.sqlite" $ do
  tryCreateTable entries
  tryCreateTable mailboxes
  tryCreateTable mailboxEntries 
  tryCreateTable feeds 
  tryCreateTable mailboxFeeds

-- dbActionsGetMailbox :: Col t Text -> Query t (Row t DbEntry)
-- dbActionsGetMailbox mailboxName = do
--    dbEnts <- select entries 
--    restrict (dbEnts ! #mailboxName .== mailboxName)
--    return dbEnts



