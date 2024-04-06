{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OneBRC.Parser
  ( LineResult(..)
  , BlockResult(..)
  , processBlock
  , processLine
  , processLineSwar
  , processLineNormal) where

import Control.Monad(when)
import Data.Bits
import Data.Word
import Foreign
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import OneBRC.Types
import OneBRC.ListHashTable

data BlockResult = BlockResult {
  _blockHeadLine :: B.ByteString,
  _blockStat :: ListHashTable, 
  _blockTailLine :: B.ByteString
  }

data LineResult  = LineResult {
  _nameEnd :: {-#UNPACK#-}!Int,
  _hashCode :: {-#UNPACK#-}!HashCode,
  _value :: {-#UNPACK#-}!Int32,
  _lineEnd ::  {-#UNPACK#-}!Int
  }deriving(Show)

--fields are index of name end, and hashCode of name
data NameResult = NameResult {-#UNPACK#-}!Int {-#UNPACK#-}!HashCode


{-# INLINE processBlock #-}
processBlock :: Int -> B.ByteString -> IO BlockResult
processBlock tableSize s@(B.BS !ptr !len) = do
  book <- newBook tableSize
  i <- B.unsafeWithForeignPtr ptr $ \p -> newLineIndexSwar p len
  pos <- go book (i + 1) len
  return $ BlockResult (B.take (min (i + 1) len) s) book (B.drop pos s)
  where
    go !book !pos !end
      | pos >= end = return pos
      | otherwise = do
          LineResult !nameEnd !hashCode !t !lineEnd <- B.unsafeWithForeignPtr ptr $ \p ->
              processLine (plusPtr p pos) (end - pos)
          if lineEnd /= -1
            then do
                 when (nameEnd /= -1) $ do
                   registerItem book (B.fromForeignPtr ptr pos nameEnd) hashCode t
                 go book (pos + lineEnd + 1) end
            else return pos

{-# INLINE processLine #-}
processLine :: Ptr Word8 -> Int -> IO LineResult
processLine !p0 !sz = processLineSwar p0 sz

{-# INLINE processLineSwar #-}
processLineSwar :: Ptr Word8 -> Int -> IO LineResult
processLineSwar !p0 !sz
  |sz >= 8 = do
     let p :: Ptr Word64 = castPtr p0
     x1 <- peekElemOff p 0
     if x1 .&. 0xff ==  0x23 -- start  with #
       then  do
         i <- newLineIndexSwar p0 sz
         return $ LineResult (-1) 0 0 i
       else do
         NameResult !k !hashCode <- pickNameSwar p sz x1
         if k >= 0
           then do let pc = plusPtr p0 (k + 1)
                   v <- fromIntegral <$> pickTemperatureSwar pc
                   i <- newLineIndexSwar pc (sz - k)
                   if i == (-1)
                     then return $ LineResult (-1) 0 0 (-1)
                     else return $ LineResult k hashCode v (i + k + 1)
           else LineResult (-1) 0 0 <$> newLineIndexSwar p0 sz
  | otherwise = processLineNormal  p0 sz

{-# INLINE pickNameSwar #-}
pickNameSwar :: Ptr Word64  -> Int -> Word64 -> IO NameResult
pickNameSwar !p !sz !x0 = go 0 x0 hashSalt
  where
    go !i !x !hashCode = do
      let !idx = commaBitIndex x
      if idx /= 0
        then do let letterCount = shiftR (countTrailingZeros idx  ) 3
                let !x' = x .&. letterMask letterCount
                return $ NameResult (shiftL i 3 + letterCount) (hashAccu hashCode x') -- i * 8 + idx / 8
        else do
          if (i `shiftL` 3) < sz
            then do let !j = i + 1
                    x1 <- peekElemOff p j
                    go j  x1 (hashAccu hashCode  x)
            else pickNameNormal (castPtr p) sz
    letterMask !i = fromIntegral $ shiftL (1::Int) (shiftL i 3) - 1
    -- letterMask 0 = 0x00
    -- letterMask 1 = 0xff
    -- letterMask 2 = 0xffff
    -- etc ..


{-# INLINE commaBitIndex #-}
commaBitIndex :: Word64 -> Word64
commaBitIndex !x = let d = xor x 0x3b3b3b3b3b3b3b3b
                   in (d - 0x0101010101010101) .&. complement d .&. 0x8080808080808080

{-# INLINE newLineBitIndex #-}
newLineBitIndex :: Word64 -> Word64
newLineBitIndex !x = let d = x `xor` 0x0a0a0a0a0a0a0a0a
                     in (d - 0x0101010101010101) .&. complement d .&. 0x8080808080808080

{-#INLINE newLineIndexSwar #-}
newLineIndexSwar :: Ptr Word8 -> Int -> IO Int
newLineIndexSwar ptr sz = go 0
  where
    p :: Ptr Word64 = castPtr ptr
    sz8 = shiftR sz 3
    go !idx
      | idx < sz8 = do
          d <- newLineBitIndex <$> peekElemOff p idx
          if d /= 0
            then return $ shiftL idx 3 + shiftR (countTrailingZeros d) 3
            else go (idx + 1)
      | otherwise = do
          let idx' = shiftL idx 3
          if idx' < sz
            then do
              q <- B.memchr (plusPtr ptr idx') newline (fromIntegral $ sz - idx')
              return $ if q == nullPtr then (-1) else idx' + q `minusPtr` ptr
            else return (-1)

{-# INLINE pickTemperatureSwar #-}
pickTemperatureSwar :: Ptr Word8 -> IO Int64
pickTemperatureSwar !p0 = do
  let p :: Ptr Int64 = castPtr p0
  d <- peekElemOff p 0
  let pos = countTrailingZeros (complement d .&. 0x10101000)
  let signed = shiftR (shiftL (complement d) 59) 63
  let designMask = complement (signed .&. 0xff)
  let aligned = shiftL (d .&. designMask) (28 - pos)
  let digits = 0x0f000f0f00 .&. aligned
  let v3 = 0x3ff .&. shiftR (digits * 0x640a0001) 32
  let v = 10 * v3 + peekNum4 (shiftR aligned 40)
  return $ xor v signed - signed
  where
    {-#INLINE peekNum4 #-}
    peekNum4 !x = let d = x `xor` 0x0a
                      mask = complement $ (d - 0x0101) .&. complement d .&. 0xff
                        -- mask = 0x00 when x = 0x0a or 0xff otherwise
                  in  (mask .&. x .&. 0x0f)

hashSalt :: HashCode
hashSalt = 1540483477 :: Word32 --0x5BD1E995
  -- 14695981039346656037 :: Word64

{-#INLINE hashAccu #-}
hashAccu :: HashCode -> Word64 -> HashCode
hashAccu !a !b =
  let h1 = m31plus a  (fromIntegral $ b .&. 0xffff)
      h2 = m31plus h1 (fromIntegral $ shiftR b 16 .&. 0xffff)
      h3 = m31plus h2 (fromIntegral $ shiftR b 32 .&. 0xffff)
  in m31plus h3 (fromIntegral $ shiftR b 48 .&. 0xffff)
  where
    m31plus !x !y = shiftL x 5 + x + y
      -- shiftL x 4 `xor` shiftR x 28 `xor` y

{-# INLINE processLineNormal #-}
processLineNormal :: Ptr Word8 -> Int -> IO LineResult
processLineNormal !p0 !sz = do
  peekElemOff p0 0 >>= \case
    0x23 -> LineResult (-1) 0 0 <$> lineEnd  p0 -- start with '#'
    _ -> do
      NameResult k hashCode <- pickNameNormal p0 sz
      if k > 0
        then do
          let pnum = p0 `plusPtr` (k + 1)
          end <- lineEnd pnum
          if end == -1
            then return $ LineResult (-1) 0 0 (-1)
            else do
              x <- readTemperature pnum end
              if x /= -2147483648
                then do
                  return $ LineResult k hashCode x end
                else
                  return $ LineResult (-1) 0 0 (-1)
        else LineResult (-1) 0 0 <$> lineEnd p0

  where
    lineEnd !p1 = do
      p <- B.memchr p1 newline (fromIntegral sz)
      return $ if p == nullPtr then -1 else p `minusPtr` p0

{-# INLINE pickNameNormal #-}
pickNameNormal :: Ptr Word8 -> Int -> IO NameResult
pickNameNormal !p0 !sz = go 0 hashSalt
  where
    go !offset !hashCode = do
      (!idx, !w) <- pickWord offset 0 0
      if | idx >= 0 -> return $ NameResult (offset + idx) (hashAccu hashCode w)
         | idx == (-1) -> go (offset + 8) (hashAccu hashCode w)
         | otherwise -> return $ NameResult (-1) 0

    pickWord !offset !i !w
      | offset + i >= sz = return (-2, 0)
      | i >= 8 = return (-1, w)
      | otherwise = do
          peekElemOff p0 (offset + i) >>= \case
            0x3b -> return (i, w)
            0x0a -> return (-2, 0)
            c -> pickWord offset (i + 1) (w .|. shiftL (fromIntegral c) (shiftL i 3))


{-# INLINE readTemperature #-}
readTemperature :: Ptr Word8 -> Int -> IO Int32
readTemperature !p0 !sz = next 0 >>= skipBlank >>= readNum
  where
    next !i
      | i >= sz = return (0, i)
      | otherwise = do
          c <- peekElemOff p0 i
          return (c, i + 1)

    readNum (!c, !i)
      | c == plus = next i >>= readInt 0
      | c == minus = neg <$> (next i >>= readInt 0)
      | isDigit c = readInt 0 (c, i)
      | otherwise = return minBound

    readInt !x (!c, !i)
      | isDigit c    = next i >>= readInt (x * 10 + c2d c)
      | c == dot     = next i >>= readFraction1 x
      | isTerminal c = return $ x * 100
      | otherwise    = return minBound

    readFraction1 !x (!c, !i)
      | isDigit c    = next i >>= readFraction2 (x * 10 + c2d c)
      | isTerminal c = return $ x * 100
      | otherwise    = return minBound

    readFraction2 !x (!c, _)
      | isDigit c = return $ x * 10 + c2d c
      | isTerminal c = return $ x * 10
      | otherwise = return minBound

    skipBlank (!c, !i)
      | c == blank  = next i >>= skipBlank
      | otherwise = return (c, i)

    isTerminal !w = w == blank || w == newline

{-# INLINE neg #-}
neg :: Int32 -> Int32
neg (-2147483648) = -2147483648
neg i = -i

{-# INLINE isDigit #-}
isDigit :: Word8 -> Bool
isDigit w = 48 <= w && w <= 57

{-# INLINE c2d #-}
c2d :: Word8 -> Int32
c2d w = fromIntegral $ w - 48

blank :: Word8
blank = 32 -- ' '

plus :: Word8
plus = 43  --  '+'

minus :: Word8
minus = 45  --  '-'

dot :: Word8
dot = 46  --  '.'

newline :: Word8
newline = 10 -- '\n'
