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

    it "fails on invalid input" $ do
      isLeft (parseJqFilter "invalid") `shouldBe` True
