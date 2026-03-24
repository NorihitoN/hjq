{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (isLeft)
import Hjq.Parser
import Test.Hspec

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
