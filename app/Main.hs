{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Run time list for 81158856 records of a 1.3G file
-- Reach 1.98 sec
-- Distribution of name length : (le 8) 20514,(between 9 15 ) 17955, (gt 16) 2874
module Main (main) where

import System.Environment
import Control.Concurrent.Async(mapConcurrently)
import Control.Monad(unless, foldM)
import Control.Exception(bracket_)
import Data.Bits
import Data.Map(toAscList)
import Foreign
import GHC.Conc (numCapabilities)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as B
import qualified Data.List as L
import qualified System.IO.MMap as MP
import System.IO
import System.Posix.Files

import OneBRC.Types
import OneBRC.ListHashTable
import OneBRC.Parser(LineResult(..), BlockResult(..), processBlock, processLine)

main :: IO ()
main = do
  fn  <- pickFilename <$> getArgs
  statis fn
  where
    pickFilename [] = error "data file name needed"
    pickFilename (s:_) = s
   
statis :: FilePath -> IO ()
statis fn = do
  book <- concurrentStat fn tableSize >>= mergeResults
  toMap book >>= printBuilder . buildResult . toAscList
  -- hashStat book >>= print
  -- hashCodeDistru book >>= print
  where
    tableSize = 65536
    printBuilder s = do
      bracket_
        (hSetBinaryMode stdout True)
        (hSetBinaryMode stdout False)
        (B.hPutBuilder stdout s)

concurrentStat :: FilePath -> Int -> IO [BlockResult]
concurrentStat fp tableSize = do
  sz <- fromIntegral . fileSize <$> getFileStatus fp
  mapConcurrently (process sz) [0 .. numCapabilities - 1]
  where
    process sz i = do
      let blockSize = sz `div` numCapabilities
      let offset = fromIntegral (i * blockSize)
      let range = if i < numCapabilities - 1
                  then (offset, blockSize)
                  else (offset, sz - i * blockSize)
      s <- MP.mmapFileByteString fp (Just range)
      processBlock tableSize s

mergeResults :: [BlockResult] -> IO ListHashTable
mergeResults [] = newBook 16
mergeResults results = do
  book <- parMergeBooks (L.map _blockStat results)

  s <- foldM (\ts0 (BlockResult hs _ ts) -> addLineMaybe book (ts0 <> hs <> "\n") >> return ts) "" results
  addLineMaybe book (s <> "\n")

  return book
  where
    addLineMaybe book (B.BS !ptr !len) = do
      LineResult !nameEnd !hashCode !t _ <- B.unsafeWithForeignPtr ptr $ \p -> processLine p len
      unless (nameEnd == -1) $ do
        let station = B.fromForeignPtr ptr 0 nameEnd
        registerItem book station hashCode t

parMergeBooks :: [ListHashTable] -> IO ListHashTable
parMergeBooks [] = error "parMergeBooks.At least 1 book required"
parMergeBooks [book] = return book
parMergeBooks [book1, book2] = mergeBook book1 book2
parMergeBooks [book1, book2, book3] = mergeBook book1 book2 >> mergeBook book1 book3
parMergeBooks books = do
  let numberedBooks = zip books [0, 1 :: Int .. ]
  let bx0 = [ b | (b, i) <- numberedBooks, i .&. 1 == 0]
  let bx1 = [ b | (b, i) <- numberedBooks, i .&. 1 /= 0]
  [book1, book2] <- mapConcurrently parMergeBooks [bx0, bx1]
  mergeBook book1 book2

buildResult :: [(B.ByteString, TemperatureStat)] -> B.Builder
buildResult = L.foldl' f mempty
  where
    f !r (!s, !t) = r <> build s t 
    build s (TemperatureStat tmax tmin total count) =
      B.byteString s <> B.char7 '/'<>
      asFloat tmax  <> B.char7 '/' <>
      asFloat tmin  <> B.char7 '/' <>
      let mean :: Float = fromIntegral total / fromIntegral count
      in asFloat (round mean :: Int32)
      <> B.char7 '/' <>
      B.intDec (fromIntegral count)  <> "\n"

    asFloat d = let (!a, !b) = d `divMod` 100
                in B.intDec (fromIntegral a) <> B.char7 '.' <> B.intDec (fromIntegral b `div` 10)

                   
