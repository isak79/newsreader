{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels #-}

module Db where

import Database.Selda
import Database.Selda.SQLite
import ParseFeed (Entry(..))

entries :: Table Entry
entries = table "entries" [#title :- primary]

dbActionsAdd :: (MonadIO m, MonadMask m) => [Entry] -> m ()
dbActionsAdd ents = withSQLite "newsreader.sqlite" $ do
  createTable entries
  insert_ entries ents
