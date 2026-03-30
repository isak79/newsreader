module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

type Title = String
type PubDate = String
type Source = String
type Entry = (Title, Source, PubDate)

main :: IO ()
main = do
  contents <- readFile "vgfeed"
  case parse parseRss "vgfeed" contents of
    Left err    -> putStrLn (errorBundlePretty err)
    Right parsed -> print parsed

parseRss :: Parser [Entry]
parseRss = do
  _ <- manyTill anySingle (string "<channel")
  _ <- many (satisfy (/= '>'))
  _ <- char '>'
  entries <- manyTill parseItem (string "</channel")
  pure entries

parseItem :: Parser Entry
parseItem = do
  _ <- manyTill anySingle (string "<item")
  _ <- many (satisfy (/= '>'))
  _ <- char '>'
  title <- parseTag "title"
  pubDate <- parseTag "pubDate"
  source <- parseTag "link"
  pure (title, source, pubDate)

parseTag :: String -> Parser String
parseTag tagName = do
  let tag = "<" ++ tagName
  _ <- manyTill anySingle (string tag)
  _ <- many (satisfy (/= '>'))
  _ <- char '>'
  let endTag = "</" ++ tagName
  contents <- manyTill anySingle (string endTag)
  pure contents
