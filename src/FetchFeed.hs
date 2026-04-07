{-# LANGUAGE OverloadedStrings #-}
module FetchFeed where

import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

fetch :: IO ()
fetch = do
  req <- parseRequest "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
  resp <- httpLBS req
  putStrLn $ "Status: " ++ show (getResponseStatusCode resp)
  L8.putStrLn (getResponseBody resp)
