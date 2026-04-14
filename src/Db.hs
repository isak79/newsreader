{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}

module Db where

import Database.Selda
import Database.Selda.SQLite
import ParseFeed (Entry(..), parseFeed)

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
  } deriving (Eq, Show, Generic)

instance SqlRow DbMailbox

mailboxes :: Table DbMailbox 
mailboxes = table "mailboxes" [#mID :- autoPrimary]

data DbFeeds = DbFeeds {
    fID :: ID DbFeeds
  , url :: URL
  } deriving (Eq, Show, Generic)

instance SqlRow DbFeeds

feeds :: Table DbFeeds
feeds = table "feeds" [ #fID :- autoPrimary
                      , #url :- unique ]

data DbMailboxFeed = DbMailboxFeed {
    mfID       :: ID DbMailboxFeed
  , feedID     :: ID DbFeeds
  , mailboxID' :: ID DbMailbox
  } deriving (Eq, Show, Generic)

instance SqlRow DbMailboxFeed

mailboxFeeds :: Table DbMailboxFeed
mailboxFeeds = table "mailboxFeeds" [ #mfID :- autoPrimary ]


data DbMailboxEntry = DbMailboxEntry {
    meID      :: ID DbMailboxEntry
  , mailboxID :: ID DbMailbox
  , entryID   :: ID DbEntry
  , isRead    :: Bool
  } deriving (Show, Eq, Generic)

instance SqlRow DbMailboxEntry 
  
mailboxEntries :: Table DbMailboxEntry
mailboxEntries = table "mailboxEntries" [#meID :- autoPrimary]

type MailboxName = Text
type URL         = Text

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

addSourceToMailbox :: MonadSelda m => URL -> MailboxName -> m ()
addSourceToMailbox url mailboxName  = do
  mids :: [ID DbMailbox] <- query $ do
    mailbox <- select mailboxes
    restrict (mailbox ! #name .== literal mailboxName)
    pure (mailbox ! #mID)

  mailboxID <- case mids of
    (x:_) -> pure x
    []    -> do insertWithPK mailboxes [DbMailbox def mailboxName]


  sids :: [ID DbFeeds] <- query $ do
    feed <- select feeds
    restrict (feed ! #url .== literal url)
    pure (feed ! #fID)

  feedID <- case sids of
    (x:_) -> pure x
    []    -> do insertWithPK feeds [DbFeeds def url]

  insert_ mailboxFeeds [DbMailboxFeed def feedID mailboxID]

-- refreshMailbox :: MonadSelda m => MailboxName -> m Int
refreshMailbox mailboxName = do
  mailboxSources <- query $ do 
    mailbox <- select mailboxes
    restrict (mailbox ! #name .== literal mailboxName)
    mailboxFeed <- select mailboxFeeds 
    restrict (mailboxFeed ! #mailboxID' .== mailbox ! #mID)
    feed <- select feeds
    restrict (feed ! #fID .== mailboxFeed ! #feedID)
    pure (feed ! #url)
  mid :: [ID DbMailbox] <- query $ do
    mailbox <- select mailboxes
    restrict (mailbox ! #name .== literal mailboxName)
    pure (mailbox ! #mID)
  feed <- liftIO $ concat <$> traverse parseFeed mailboxSources
  let dbFeed = map toDbEntry feed
  entryIDs <- mapM (insertWithPK entries) $ map (\x -> [x]) dbFeed
  insert_ mailboxEntries (map (dbMailboxEntry (head mid)) entryIDs)
    where
      dbMailboxEntry :: ID DbMailbox -> ID DbEntry -> DbMailboxEntry
      dbMailboxEntry mailboxID entryID = DbMailboxEntry def mailboxID entryID False
      


addSourceAndMailbox :: MonadSelda m => MailboxName -> URL -> m ()
addSourceAndMailbox mailboxName url = do
  mailboxID <- insertWithPK mailboxes [DbMailbox def mailboxName]
  sourceID <- insertWithPK feeds [DbFeeds def url]
  insert_ mailboxFeeds [DbMailboxFeed def sourceID mailboxID]

getEntries :: MonadSelda m => MailboxName -> m [Entry]
getEntries mailboxName = do
  dbEntries <- query $ do
    mailbox <- select mailboxes
    restrict (mailbox ! #name .== literal mailboxName)
    mailboxEntry <- select mailboxEntries
    restrict (mailboxEntry ! #mailboxID .== mailbox ! #mID)
    entry <- select entries
    restrict (entry ! #eID .== mailboxEntry ! #entryID)
    pure entry
  pure (map fromDbEntry dbEntries)
  

initializeTables :: IO ()
initializeTables = withSQLite "newsreader.sqlite" $ do
  tryCreateTable entries
  tryCreateTable mailboxes
  tryCreateTable mailboxEntries 
  tryCreateTable feeds 
  tryCreateTable mailboxFeeds

