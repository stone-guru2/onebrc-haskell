{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OneBRC.ListHashTable (ListHashTable(..), hashCodeDistru) where

import Control.Monad
import Data.Bits
import Data.Int
import qualified Data.List as L
import qualified Data.Vector.Mutable as MV
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Primitive.ByteArray
import Control.Monad.Primitive(RealWorld)

import OneBRC.Types

newtype ListHashTable = ListHashTable {
  _itemVec :: MV.IOVector [TemperatureItem]
  }

data TemperatureItem = TemperatureItem {
  _itemHashCode ::{-# UNPACK #-} !HashCode,
  _itemStation :: {-# UNPACK #-} !B.ByteString,
  _itemValues ::  {-# UNPACK #-} !(MutableByteArray RealWorld)
  }

instance TemperatureBook ListHashTable where
  newBook cap 
    | cap == cap .&. (complement cap +  1) = do
        vec <- MV.replicate cap []
        return $ ListHashTable vec
    | otherwise = error "capacity of hash book should be pow of 2"
    -- 
  {-# INLINE registerItem #-}
  registerItem book !station !hashCode !t = do
    mutate book station hashCode (addTemperature t) (newItem station hashCode t)

  mergeBook book1 book2 = MV.mapM_ merge (_itemVec book2) >> return book1
    where
      merge rx = forM_ rx (\i@(TemperatureItem hashCode station _) ->
                             mutate book1 station hashCode (mergeItem i) (cloneItem i))

  lookupItem table station hashCode = do
    innerLookup table station hashCode >>= \case 
      Right item -> Just <$> itemToStat item
      Left _ -> return Nothing

  toMap (ListHashTable vec) = MV.foldM' f Map.empty vec
    where
      f = foldM $ \m item -> do
        t <- itemToStat item
        return $ Map.insert (_itemStation item) t m
  
  hashStat = listHashTableStat

{-# INLINE mutate #-}
mutate :: ListHashTable -> B.ByteString -> HashCode
         -> (TemperatureItem -> IO ())
         -> IO TemperatureItem
         -> IO ()
mutate table@(ListHashTable vec) !station !hashCode p q = do
  innerLookup table station hashCode >>= \case
    Right item -> p item
    Left index -> do
      item <- q
      MV.unsafeModify vec (item :) index

{-# INLINE innerLookup #-}
innerLookup :: ListHashTable -> B.ByteString -> HashCode -> IO (Either Int TemperatureItem)
innerLookup (ListHashTable vec) !station !hashCode = MV.read vec index >>= go
    where
      index = hashToIndex (MV.length vec) hashCode
      go [] = return (Left index)
      go (item@(TemperatureItem c s _):ix)
        | c == hashCode && s == station = return (Right item)
        | otherwise = go ix

{-#INLINE hashToIndex #-}
hashToIndex :: Int -> HashCode -> Int
hashToIndex !capacity !hashCode =
  let d = hashCode + shiftR hashCode 7 + shiftR hashCode 15 + shiftR hashCode 23
  in fromIntegral d .&. (capacity - 1)
  

{-# INLINE newItem #-}
newItem :: B.ByteString -> HashCode-> Int32 -> IO TemperatureItem
newItem !s !hashCode !t = do
  vec <- newByteArray 16
  setByteArray vec 0 3 t
  writeByteArray vec 3 (1 :: Int32)
  return $ TemperatureItem hashCode s vec

{-# INLINE addTemperature #-}
addTemperature :: Int32 -> TemperatureItem -> IO ()
addTemperature !t (TemperatureItem _ _ vec) = do
  tmax <- readByteArray vec 0
  writeByteArray vec 0 (maxi tmax t)
  tmin <- readByteArray vec 1
  writeByteArray vec 1 (mini tmin t)

  tsum :: Int64 <- readByteArray vec 1 -- the second 64 bit element 
  writeByteArray vec 1 (tsum + sum1 t)
  where
    {-# INLINE sum1 #-}
    sum1 :: Int32 -> Int64
    sum1 !x = fromIntegral x .|. 0x100000000
    {-# INLINE mini #-}
    maxi :: Int32 -> Int32 -> Int32
    maxi !a !b = let c = - (fromIntegral $ fromEnum (a < b))
                 in a `xor` ((a `xor` b) .&. c)
    {-# INLINE maxi #-}
    mini :: Int32 -> Int32 -> Int32
    mini !a !b = let c = - (fromIntegral $ fromEnum (a < b))
                 in b `xor` ((a `xor` b) .&. c)

{-# INLINE cloneItem #-}  
cloneItem :: TemperatureItem -> IO TemperatureItem  
cloneItem (TemperatureItem hashCode station vec) =
  TemperatureItem hashCode station  <$> cloneMutableByteArray vec 0 16

{-# INLINE itemToStat #-}
itemToStat :: TemperatureItem -> IO TemperatureStat
itemToStat (TemperatureItem _ _  vec) = do
  tmax :: Int32 <- readByteArray vec 0
  tmin :: Int32 <- readByteArray vec 1
  tsum :: Int32  <- readByteArray vec 2
  tcount :: Int32 <- readByteArray vec 3
  return $ TemperatureStat tmax tmin tsum tcount

{-# INLINE mergeItem #-}
mergeItem :: TemperatureItem -> TemperatureItem -> IO ()
mergeItem (TemperatureItem _ _ vec2) (TemperatureItem _ _ vec1) = do
  tmax2 :: Int32 <- readByteArray vec2 0
  tmax1 :: Int32 <- readByteArray vec1 0
  writeByteArray vec1 0 (max tmax2 tmax1)

  tmin2 :: Int32 <- readByteArray vec2 1
  tmin1 :: Int32 <- readByteArray vec1 1
  writeByteArray vec1 1 (min tmin2 tmin1)

  suma :: Int64 <- readByteArray vec2 1
  sumb :: Int64 <- readByteArray vec1 1
  writeByteArray vec1 1 (suma + sumb)

-- | Statistics of Hash code collision, in form of : (t,  n,  r)
-- 
-- where t is the hash code occur times of different station,
-- n is how many hash code having this occurance,
-- r they overall ratio of the n
hashCodeDistru :: ListHashTable -> IO [(Int, Int, Float)]
hashCodeDistru (ListHashTable vec) = do
  codeOccur <- Map.toList <$> MV.foldl' f Map.empty vec  -- (hashCode, num)
  let codeTotal = L.foldl' (\n (_, m) -> n + m) 0 codeOccur -- Total num of hashCode
  return $ addRatio  (Map.toList $ codeOccurDistru codeOccur) codeTotal
  where
    addRatio occurDist total = L.map (\(occur, num) ->
                                         (occur, num, fromIntegral num / fromIntegral total))
                               occurDist
    codeOccurDistru codeOccur = L.foldl' (\m (_, num) -> Map.alter incf num m) Map.empty codeOccur
    f m0 rx = L.foldl' (\m1 i -> Map.alter incf (_itemHashCode i) m1) m0 rx
    incf Nothing = Just 1
    incf (Just n) = Just (n + 1)

listHashTableStat :: ListHashTable -> IO HashStat
listHashTableStat (ListHashTable vec) = do
  count <- MV.foldM' (\s rx -> return $ s + L.length rx) 0 vec
  m <- MV.foldM' (addToMap count) Map.empty vec
  let rx = Map.toList m
  let lenRatioList = L.map (\(a, (b, c)) -> HashSlotStat a b c) rx
  return $ HashStat lenRatioList (loadRatio (MV.length vec) rx) (calcuScore rx)
    where
      addToMap count m0 rx = do
        let len = L.length rx
        return $ Map.alter (alterMap count len) len m0

      alterMap count len maybeLen =
        let ratio = 100.0 * fromIntegral len / fromIntegral count
        in case maybeLen of
             Nothing -> Just (1,  ratio)
             Just (n, r) -> Just (n + 1, r + ratio)

      calcuScore rx =
        let score = L.foldl (\s (len, (_, ratio)) -> s + fromIntegral len * ratio) 0.0 rx
        in  score / 100.0

      loadRatio cap rx = 1.0 - emptyRatio
        where emptyRatio = maybe 0.0 (\(n, _) -> fromIntegral n / fromIntegral cap) (L.lookup 0 rx)
