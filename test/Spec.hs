{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (isLeft)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Hjq.Parser
import Hjq.Query (applyFilter)
import Test.Hspec
import Data.Aeson (Value(..))

main :: IO ()
main = hspec $ do
  describe "parseJqFilter" $ do
    it "parses identity (.)" $ do
      parseJqFilter "." `shouldBe` Right JqNil

    it "parses single field (.foo)" $ do
      parseJqFilter ".foo" `shouldBe` Right (JqField "foo" JqNil)

    it "parses array index (.[0])" $ do
      parseJqFilter ".[0]" `shouldBe` Right (JqIndex 0 JqNil)

    it "parses index and field (.[0].foo)" $ do
      parseJqFilter ".[0].foo" `shouldBe` Right (JqIndex 0 (JqField "foo" JqNil))

    it "parses nested fields (.foo.bar)" $ do
      parseJqFilter ".foo.bar" `shouldBe` Right (JqField "foo" (JqField "bar" JqNil))

    it "parses field and index (.foo[0])" $ do
      parseJqFilter ".foo[0]" `shouldBe` Right (JqField "foo" (JqIndex 0 JqNil))

    it "fails on invalid input" $ do
      isLeft (parseJqFilter "invalid") `shouldBe` True

  describe "parseJqFilter with space" $ do
    it "parses identity with leading space and trailing space ( . )" $ do
      parseJqFilter " . " `shouldBe` Right JqNil

    it "parses field with leading space (. foo)" $ do
      parseJqFilter ". foo " `shouldBe` Right (JqField "foo" JqNil)

    it "parses field with trailing space (. [0] . field)" $ do
      parseJqFilter ". [0] . fieldName" `shouldBe` Right (JqIndex 0 (JqField "fieldName" JqNil))

    it "parses (. fieldName [ 0 ])" $ do
      parseJqFilter " . fieldName [ 0 ] " `shouldBe` Right (JqField "fieldName" (JqIndex 0 JqNil))

    it "parses (. foo . bar )" $ do
      parseJqFilter " . foo . bar " `shouldBe` Right (JqField "foo" (JqField "bar" JqNil))

  describe "parseJqQuery" $ do
    it "parses empty array" $ do
      parseJqQuery "[]" `shouldBe` Right (JqQueryArray [])

    it "parses field and array" $ do
      parseJqQuery "[.foo,.bar]" `shouldBe` Right (JqQueryArray [JqQueryFilter (JqField "foo" JqNil), JqQueryFilter (JqField "bar" JqNil)])

    it "parses object with fields" $ do
      parseJqQuery "{\"foo\":[],\"bar\":[]}" `shouldBe` Right (JqQueryObject [("foo", JqQueryArray []), ("bar", JqQueryArray [])])

  describe "parseJqQuery with space" $ do
    it "parses empty array with space" $ do
      parseJqQuery " [ ] " `shouldBe` Right (JqQueryArray [])

    it "parses field and array with space" $ do
      parseJqQuery " [ .foo , .bar ] " `shouldBe` Right (JqQueryArray [JqQueryFilter (JqField "foo" JqNil), JqQueryFilter (JqField "bar" JqNil)])

    it "parses object with fields and space" $ do
      parseJqQuery " { \"foo\" : [ ] , \"bar\" : [ ] } " `shouldBe` Right (JqQueryObject [("foo", JqQueryArray []), ("bar", JqQueryArray [])])

  describe "applyFilter" $ do
    it "identity (.)" $
      applyFilter (unsafeParseFilter ".") testData `shouldBe` Right testData

    it "field access (.string-field)" $
      applyFilter (unsafeParseFilter ".string-field") testData `shouldBe` Right (String "string value")

    it "nested field (.nested-field.inner-string)" $
      applyFilter (unsafeParseFilter ".nested-field.inner-string") testData `shouldBe` Right (String "inner value")

    it "nested number (.nested-field.inner-number)" $
      applyFilter (unsafeParseFilter ".nested-field.inner-number") testData `shouldBe` Right (Number 100)

    it "array index 0 (.array-field[0])" $
      applyFilter (unsafeParseFilter ".array-field[0]") testData `shouldBe` Right (String "first field")

    it "array index 1 (.array-field[1])" $
      applyFilter (unsafeParseFilter ".array-field[1]") testData `shouldBe` Right (String "next field")

    it "object in array (.array-field[2].object-in-array)" $
      applyFilter (unsafeParseFilter ".array-field[2].object-in-array") testData `shouldBe` Right (String "string value in object-in-array")

-- テスト用データ
testData :: Value
testData =
  Object $ KM.fromList
    [ ("string-field", String "string value")
    , ("nested-field", Object $ KM.fromList
        [ ("inner-string", String "inner value")
        , ("inner-number", Number 100)
        ])
    , ("array-field", Array $ V.fromList
        [ String "first field"
        , String "next field"
        , Object $ KM.fromList [("object-in-array", String "string value in object-in-array")]
        ])
    ]

unsafeParseFilter :: Text -> JqFilter
unsafeParseFilter t = case parseJqFilter t of
  Right f  -> f
  Left err -> error ("PARSE FAILURE IN A TEST" ++ T.unpack err)
