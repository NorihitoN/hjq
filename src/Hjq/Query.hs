{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Hjq.Query where

import Data.Aeson (Value (Array, Object))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Vector as V
import Hjq.Parser

applyFilter :: JqFilter -> Value -> Either T.Text Value
applyFilter JqNil value = Right value
applyFilter (JqField name next) (Object obj) =
  case KM.lookup (Key.fromText name) obj of
    Just v -> applyFilter next v
    Nothing -> Left $ "Field '" <> name <> "' not found in object"
applyFilter (JqIndex i next) (Array arr) =
  if i >= 0 && i < V.length arr
    then
      applyFilter next (arr V.! i)
    else
      Left $ "Index " <> T.pack (show i) <> " out of bounds for array of length " <> T.pack (show (V.length arr))
applyFilter (JqField name _) val = Left $ "Cannot apply field '" <> name <> "' to non-object value: " <> T.pack (show val)
applyFilter (JqIndex i _) val = Left $ "Cannot apply index " <> T.pack (show i) <> " to non-array value: " <> T.pack (show val)

executeQuery :: JqQuery -> Value -> Either T.Text Value
executeQuery (JqQueryObject o) v =
  Object . KM.fromList <$> traverse (\(k, q) -> (Key.fromText k,) <$> executeQuery q v) o
executeQuery (JqQueryArray arr) v =
  Array . V.fromList <$> traverse (`executeQuery` v) arr
executeQuery (JqQueryFilter f) v = applyFilter f v
