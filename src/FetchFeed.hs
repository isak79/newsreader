{-# LANGUAGE OverloadedStrings #-}
module FetchFeed(fetchFeed) where

import Text.Feed.Import
import Text.Feed.Types
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

fetchBytes :: String -> IO L8.ByteString
fetchBytes url = do
  req <- parseRequest url
  resp <- httpLBS req
  putStrLn $ "Status: " ++ show (getResponseStatusCode resp)
  pure (getResponseBody resp)

fetchFeed :: String -> IO (Maybe Feed)
fetchFeed url = do
  parseFeedSource <$> fetchBytes url
