module Hjq.Parser where

import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

data JqFilter
  = JqField Text JqFilter
  | JqIndex Int JqFilter
  | JqNil
  deriving (Show, Read, Eq)

data JqQuery
  = JqQueryObject [(Text, JqQuery)]
  | JqQueryArray [JqQuery]
  | JqQueryFilter JqFilter
  deriving (Show, Read, Eq)

parseJqQuery :: Text -> Either Text JqQuery
parseJqQuery input =
  case parse queryP "" input of
    Left err -> Left $ T.pack (errorBundlePretty err)
    Right q -> Right q

queryP :: Parser JqQuery
queryP = try queryObjectP <|> try queryArrayP <|> queryFilterP

queryArrayP :: Parser JqQuery
queryArrayP = do
  sc
  _ <- char '['
  sc
  queries <- queryP `sepBy` try (sc *>  char ',' *> sc)
  sc
  _ <- char ']'
  return $ JqQueryArray queries

queryObjectP :: Parser JqQuery
queryObjectP = do
  sc
  _ <- char '{'
  sc
  pairs <- qObj `sepBy` try (sc *> char ',' *> sc)
  sc
  _ <- char '}'
  sc
  return $ JqQueryObject pairs
  where
    qObj = do
      key <- char '"' *> takeWhile1P (Just "object key") isAlpha <* char '"'
      sc
      _ <- char ':'
      sc
      value <- queryP
      return (key, value)

queryFilterP :: Parser JqQuery
queryFilterP = do JqQueryFilter <$> filterP

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter input =
  case parse (filterP <* eof) "" input of
    Left err -> Left $ T.pack (errorBundlePretty err)
    Right f -> Right f

sc :: Parser ()
sc = space

filterP :: Parser JqFilter
filterP = do
  sc *> dotP

fieldP :: Parser JqFilter
fieldP = do
  name <- takeWhile1P (Just "filed name") isAlpha
  sc
  rest <- dotP <|> indexP <|> return JqNil
  return $ JqField name rest

dotP :: Parser JqFilter
dotP = do
  _ <- char '.'
  sc
  fieldP <|> indexP <|> return JqNil

indexP :: Parser JqFilter
indexP = do
  _ <- char '['
  sc
  n <- decimal
  sc
  _ <- char ']'
  sc
  rest <- dotP <|> return JqNil
  return $ JqIndex n rest
