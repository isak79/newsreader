module FetchFeed where
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import qualified Data.ByteString.Builder as L8
import Network.HTTP.Client.Conduit

fetch = do
  initReq      <- parseRequest "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
  httpJSON initReq
