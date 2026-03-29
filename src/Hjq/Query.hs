module Hjq.Query where

import Data.Aeson (Value)
import Data.Text (Text)
import Hjq.Parser (JqFilter)

applyFilter :: JqFilter -> Value -> Either Text Value
applyFilter = undefined
