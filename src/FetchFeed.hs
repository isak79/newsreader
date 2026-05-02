{-# LANGUAGE OverloadedStrings #-}
module FetchFeed(fetchFeed, fetchBytes) where

import Text.Feed.Import
import Text.Feed.Types
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text

fetchBytes :: Text -> IO L8.ByteString
fetchBytes url = do
  req <- parseRequest $ unpack url
  resp <- httpLBS req
  pure (getResponseBody resp)

fetchFeed :: Text -> IO (Maybe Feed)
fetchFeed url = do
  parseFeedSource <$> fetchBytes url
