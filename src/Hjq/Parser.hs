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

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter input =
  case parse filterP "" input of
    Left err -> Left $ T.pack (errorBundlePretty err)
    Right f -> Right f

filterP :: Parser JqFilter
filterP = do
  dotP <* eof

fieldP :: Parser JqFilter
fieldP = do
  name <- takeWhile1P (Just "filed name") isAlpha
  rest <- dotP <|> return JqNil
  return $ JqField name rest

dotP :: Parser JqFilter
dotP = do
  _ <- char '.'
  fieldP <|> indexP <|> return JqNil

indexP :: Parser JqFilter 
indexP = do 
  _ <- char '['
  n <- decimal
  _ <- char ']'
  rest <- dotP <|> return JqNil 
  return $ JqIndex n rest 
