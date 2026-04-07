{-# LANGUAGE OverloadedStrings #-}
module FetchFeed(fetchFeed) where

import Text.Feed.Import
import Text.Feed.Types
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

fetchBytes :: IO L8.ByteString
fetchBytes = do
  req <- parseRequest "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
  resp <- httpLBS req
  putStrLn $ "Status: " ++ show (getResponseStatusCode resp)
  pure (getResponseBody resp)

fetchFeed :: IO (Maybe Feed)
fetchFeed = do
  fmap parseFeedSource fetchBytes 
