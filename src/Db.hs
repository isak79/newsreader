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
entries = table "entries" [#eID :- autoPrimary]

data DbMailbox = DbMailbox {
    mID           :: ID DbMailbox 
  , url           :: Text
  , name          :: Text
  , unreadEntries :: Int
  } deriving (Eq, Show, Generic)

instance SqlRow DbMailbox

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



-- dbActionsAdd :: (MonadIO m, MonadMask m) => Text -> [Entry] -> m ()
-- dbActionsAdd mailboxName ents = withSQLite "newsreader.sqlite" $ do
--   let dbEnts = map (toDbEntry mailboxName) ents
--   createTable entries
  -- insert_ entries dbEnts

-- dbActionsGetMailbox :: Col t Text -> Query t (Row t DbEntry)
-- dbActionsGetMailbox mailboxName = do
--    dbEnts <- select entries 
--    restrict (dbEnts ! #mailboxName .== mailboxName)
--    return dbEnts



