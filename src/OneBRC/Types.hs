{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OneBRC.Types
    ( HashCode
    , TemperatureStat(..)
    , TemperatureBook(..)
    , HashSlotStat(..)
    , HashStat(..)
    ) where

import Data.Word
import Foreign
import qualified Data.ByteString as B
import Text.Printf
import qualified Data.Map as Map

type HashCode = Word32

data TemperatureStat = TemperatureStat {
  _tmax :: {-# UNPACK #-}!Int32,
  _tmin :: {-# UNPACK #-}!Int32,
  _ttotal :: {-# UNPACK #-} !Int32,
  _tcount :: {-# UNPACK #-} !Int32
  }deriving(Show)

data HashSlotStat = HashSlotStat {
  _hashSlotSize :: Int, -- How many elements in the slot
  _hashSlotItemNum :: Int,  -- Total items in these slots having _hashSlotSize size
  _hashSlotItemRatio :: Double -- (My _hashSlotItemNum) / (num of all elements)
  }

instance Show HashSlotStat where
  show (HashSlotStat a b c) = printf "(%d, %d, %.3f%%)" a b c
  
data HashStat = HashStat {
  _hashSlotStats :: [HashSlotStat],
  _hashLoadRatio :: Double,
  _hashAvgDist :: Double
  }deriving(Show)

class TemperatureBook a where
  newBook :: Int -> IO a
  mergeBook :: a -> a -> IO a
  registerItem :: a -> B.ByteString -> HashCode -> Int32 -> IO ()
  lookupItem :: a -> B.ByteString -> HashCode -> IO (Maybe TemperatureStat)
  toMap :: a -> IO (Map.Map B.ByteString TemperatureStat)
  hashStat :: a -> IO HashStat 
  
