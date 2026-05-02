{-# LANGUAGE OverloadedStrings #-}
module ParseNews where

import qualified Readability as R
import qualified Text.XML as X
import qualified Text.XML.Cursor as C
import FetchFeed
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

handleNews = do
  bytes <- fetchBytes "https://www.nrk.no/ostfold/falt-i-elv-i-halden-_-livreddende-forstehjelp-pagar-1.17869123"
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
      cleanContent = T.unlines . map T.strip . map T.unlines . map (filter (not . T.null))
      filteredContent = cleanContent $ map snd $ filter (\(t,_) -> t `elem` wantedTags) tagCont 
  TIO.putStrLn filteredContent 

