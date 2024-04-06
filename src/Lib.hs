{-# LANGUAGE BangPatterns #-}
module Lib
    ( readTemperature,
      wordBinary,
      wordBinary32,
      processLineBuildIn,
      strToInt,
      strToInt2,
      strToInt3
    ) where

import Data.Word
import Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import qualified Data.List as L
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
import Data.Hashable
import Foreign

import OneBRC.Parser(LineResult(..))

strToInt2 :: String -> Int
strToInt2 = go 0
  where
    go a [] = a
    go a (c:cx) = go (10 * a + c2i c) cx
    c2i c = fromEnum c - 48

strToInt3 :: B.ByteString -> Int
strToInt3 = B.foldl' (\a c -> a * 10 + c2i c) 0
  where c2i c = fromIntegral (c - 48)

strToInt :: B.ByteString -> Int
strToInt (B.BS ptr sz)
  | sz == 8 = unsafePerformIO $ withForeignPtr ptr readAndConvert
  | otherwise = error "unsupported"
  where
    readAndConvert p = convert <$> peekElemOff (castPtr p) 0
    convert :: Word64 -> Int
    convert !w = let w1 = w .&. 0x0F0F0F0F0F0F0F0F
                     w2 = w1 .&. 0x000F000F000F000F * 10 + (shiftR w1 8) .&. 0x000F000F000F000F
                     w3 = w2 .&. 0x0000007F0000007F * 100 + (shiftR w2 16) .&. 0x0000007F0000007F
                 in fromIntegral $ (w3 .&. 0x3FFF) * 10000 + (shiftR w3 32) .&. 0x3FFF
                   
wordBinary32 :: (Integral a, PrintfArg a) => a -> String
wordBinary32 w = L.tail $ go (printf "%032b" w) []
  where
    go [] sx = L.reverse sx
    go bx sx = let (charBits, lx) = L.splitAt 8 bx
               in go lx (L.reverse charBits ++ ('-' : sx))


wordBinary :: (Integral a, PrintfArg a) => a -> String
wordBinary w = go (printf "%064b" w) []
  where
    go [] sx = L.reverse sx
    go bx sx = let (charBits, lx) = L.splitAt 8 bx
               in go lx (L.reverse charBits ++ ('-' : sx))
  
errorResult :: LineResult
errorResult = LineResult (-1) 0 0 (-1)

processLineBuildIn :: B.ByteString -> LineResult
processLineBuildIn s
  | B.null s = errorResult
  | otherwise = if B.unsafeHead s == 0x23
                then errorResult
                else case B.elemIndex comma s of
                       Nothing -> errorResult
                       Just commaIndex -> let s' = B.drop commaIndex s
                                              code = hash (B.take commaIndex s)
                                              lineEnd = maybe (-1) id (B.elemIndex newline s)
                                          in case Lib.readTemperature  s' of
                                               Nothing -> errorResult
                                               Just v -> LineResult commaIndex (fromIntegral code) v lineEnd
  where
    newline = 10

{-# INLINE readTemperature #-}
readTemperature :: B.ByteString -> Maybe Int32
readTemperature str
  | B.null str = Nothing
  | otherwise = readNum (skipBlank str)
  where
    readNum s
      | B.null s = Nothing
      | B.unsafeHead s == plus = readDigit (B.unsafeTail s)
      | B.unsafeHead s == minus = neg (readDigit $ B.unsafeTail s)
      | otherwise = readDigit s
  
    readDigit s
      | B.null s = Nothing
      | isDigit (B.unsafeHead s) = readInt (w2d (B.unsafeHead s)) (B.unsafeTail s)
      | otherwise = Nothing

    readInt x s
      | B.null s = Just (x * 100)
      | isBlank (B.unsafeHead s) = Just (x * 100)
      | isDigit (B.unsafeHead s) = readInt (x * 10 + w2d (B.unsafeHead s)) (B.unsafeTail s)
      | B.unsafeHead s == dot = readFraction1 x (B.unsafeTail s)
      | otherwise = error $ show s
      
    readFraction1 x s
      | B.null s = Just (x * 100)
      | isBlank (B.unsafeHead s) = Just (x * 100)
      | isDigit (B.unsafeHead s) = readFraction2 (x * 10 + w2d (B.unsafeHead s)) (B.unsafeTail s)
      | otherwise = Nothing

    readFraction2 x s
      | B.null s = Just (x * 10)
      | isBlank (B.unsafeHead s) = Just (x * 10)
      | isDigit (B.unsafeHead s) = return (x * 10 + w2d (B.unsafeHead s))
      | otherwise = Nothing
      
    skipBlank s
      | B.null s = s
      | otherwise = let c = B.unsafeHead s
                    in if c == comma || c == blank
                       then skipBlank (B.unsafeTail s)
                       else s

comma :: Word8
comma = 59 -- fromEnum ';'
  
blank :: Word8
blank = 32 -- fromEnum ' '
  
plus :: Word8
plus = 43
  
minus :: Word8
minus = 45

dot :: Word8
dot = 46

{-# INLINE neg #-}
neg :: Maybe Int32 -> Maybe Int32  
neg Nothing = Nothing
neg (Just i) = Just (-i)

{-# INLINE isDigit #-}
isDigit :: Word8 -> Bool
isDigit w = 48 <= w && w <= 57

{-# INLINE w2d #-}
w2d :: Word8 -> Int32
w2d w = fromIntegral $ w - 48

{-# INLINE isBlank #-}
isBlank :: Word8 -> Bool
isBlank w = w == comma || w == blank 
