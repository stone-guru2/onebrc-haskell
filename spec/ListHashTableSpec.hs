{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module ListHashTableSpec  where

import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Map as Map

import Test.Hspec
import OneBRC.Types
import OneBRC.ListHashTable

hashb :: B.ByteString -> HashCode
hashb s
  | B.null s = 0
  | otherwise = fromIntegral (B.head s)
  
spec :: Spec
spec = do
  describe "list hash basic test" $ do
    it "registerItem and found" $ do
      book :: ListHashTable <- newBook 32
      insertItem book "station1"
      r <- searchItem book "station1"
      isJust r `shouldBe` True
      
      
    it "registerItem and not found" $ do
      book :: ListHashTable <- newBook 32
      insertItem book "station1"
      r <- searchItem book "station2"
      isJust r `shouldBe` False

  describe "list  hash when conflict " $ do
    it "registerItem and found" $ do
      book :: ListHashTable <- newBook 32
      insertItem book "station1"
      insertItem book "station2"
      insertItem book "tation1"  -- "t" is "s" + 1
      insertItem book "tation2"
      -- printSlots book
      r1 <- searchItem book "station1"
      isJust r1 `shouldBe` True
      r2 <- searchItem book "station2"
      isJust r2 `shouldBe` True
      r3 <- searchItem book "tation1"
      isJust r3 `shouldBe` True
      
    it "registerItem and not found" $ do
      book :: ListHashTable <- newBook 32
      insertItem book "station1"
      insertItem book "station2"
      insertItem book "tation1"  -- "t" is "s" + 1
      insertItem book "tation2"
      -- printSlots book
      r1 <- searchItem book "station3"
      isJust r1 `shouldBe` False
      r3 <- searchItem book "tation3"
      isJust r3 `shouldBe` False

  describe "toMap " $ do
    it "some item" $ do
      book :: ListHashTable <- newBook 32
      insertItem book "station1"
      insertItem book "station2"
      insertItem book "tation1"  
      insertItem book "tation2"
      rx <- toMap book
      Map.size rx `shouldBe` 4
  
    it "empty book" $ do
      book :: ListHashTable <- newBook 32
      rx <- toMap book
      Map.size rx `shouldBe` 0
  
      -- printSlots book      
insertItem :: ListHashTable -> B.ByteString -> IO ()
insertItem table s = registerItem table s (hashb s) 2300

searchItem :: ListHashTable -> B.ByteString -> IO (Maybe TemperatureStat)gst
searchItem table s = lookupItem table s (hashb s)
