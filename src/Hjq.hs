{-# LANGUAGE OverloadedStrings #-}

module Hjq (hjq) where

import Control.Error.Util (note)
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Hjq.Parser (parseJqQuery)
import Hjq.Query (executeQuery)

hjq :: B.ByteString -> T.Text -> Either T.Text B.ByteString
hjq jsonString queryString = do
  value <- note "Invalid json format." $ decode jsonString
  query <- parseJqQuery queryString
  encodePretty <$> executeQuery query value
