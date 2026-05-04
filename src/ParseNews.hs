{-# LANGUAGE OverloadedStrings #-}
module ParseNews(handleNews) where

import qualified Readability as R
import qualified Text.XML as X
import qualified Text.XML.Cursor as C
import FetchFeed
import Data.Maybe
import qualified Data.Text as T
import Data.List (nub)

-- | Takes a url as argument, return the article contents
handleNews :: T.Text -> IO T.Text
handleNews url = do
  bytes <- fetchBytes url
  let art = (fromJust . R.fromByteString) bytes
      doc = R.summary art
      cursor = C.fromDocument doc
      desc = cursor C.$// C.descendant 
      tagName cur = case C.node cur of
        X.NodeElement el -> X.nameLocalName (X.elementName el)
        _                -> "<no-element>"
      tagNames = map tagName desc
      content = map (\cur -> cur C.$// C.content) desc
      tagCont = zip tagNames content
      wantedTags = ["h1","h2","h3","p","li"]
      cleanContent = map T.strip . map T.unlines . map (filter (not . T.null))
      filteredContent = T.unlines $ nub $ cleanContent $ map snd $ filter (\(t,_) -> t `elem` wantedTags) tagCont 
  pure filteredContent 

