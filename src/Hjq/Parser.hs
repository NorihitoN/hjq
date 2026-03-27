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

lexeme :: Parser a -> Parser a
lexeme = (<* space)

symbol :: Char -> Parser Char
symbol = lexeme . char

parseWith :: Parser a -> Text -> Either Text a
parseWith p input =
  case parse (sc *> p <* eof) "" input of
    Left err -> Left $ T.pack (errorBundlePretty err)
    Right res -> Right res

parseJqQuery :: Text -> Either Text JqQuery
parseJqQuery = parseWith queryP

queryP :: Parser JqQuery
queryP = queryObjectP <|> queryArrayP <|> queryFilterP

queryArrayP :: Parser JqQuery
queryArrayP = do
  JqQueryArray <$> between (symbol '[') (symbol ']') (queryP `sepBy` symbol ',')

queryObjectP :: Parser JqQuery
queryObjectP = JqQueryObject <$> between (symbol '{') (symbol '}') (keyValueP `sepBy` symbol ',')
  where
    keyValueP =
      (,)
        <$> (symbol '"' *> lexeme (takeWhile1P (Just "object key") isAlpha) <* symbol '"')
        <* symbol ':'
        <*> queryP

queryFilterP :: Parser JqQuery
queryFilterP = JqQueryFilter <$> filterP

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter = parseWith filterP

sc :: Parser ()
sc = space

filterP :: Parser JqFilter
filterP = dotP

fieldP :: Parser JqFilter
fieldP =
  JqField
    <$> lexeme (takeWhile1P (Just "field name") isAlpha)
    <*> (dotP <|> indexP <|> return JqNil)

dotP :: Parser JqFilter
dotP =
  symbol '.'
    *> (fieldP <|> indexP <|> return JqNil)

indexP :: Parser JqFilter
indexP =
  JqIndex
    <$> between (symbol '[') (symbol ']') (lexeme decimal)
    <*> (dotP <|> return JqNil)
