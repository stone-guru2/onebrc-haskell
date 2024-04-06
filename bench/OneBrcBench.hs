{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import Criterion.Main

import Data.Int
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Foreign
  
import OneBRC.Parser(LineResult(..), processLineSwar, processLineNormal)
import qualified Lib as Lib

tempeLines :: [B.ByteString]
tempeLines = ["Tokyo;35.6897\notherthing;10.12"
        , "Jakarta;-6.1750\notherthing;10.12"
        , "Boston;2.6\nothering;23.123"
        , "Guangzhou;23.1300\notherthing;10.12"
        , "Mumbai;19.0761\notherthing;10.12"
        , "Manila Long Long Long;14.5958\notherthing;10.12"
        , "Shanghai Long;31.1667\notherthing;10.12"
        , "São Paulo;-23.5500\notherthing;10.12"]

tempeLines2 :: [B.ByteString]
tempeLines2 = L.map makeLine (L.replicate 10 128)
  where
    makeLine n = B.replicate (fromEnum 'B') n <> ";23.35\nother thing;12.3566"

swarParse :: [B.ByteString] -> IO Int32
swarParse sx = do
  tx <- mapM (\(B.BS ptr len) -> withForeignPtr ptr $ \p -> processLineSwar p len) sx
  return $ L.foldl' (\s r -> s + _value r) 0 tx

normalParse :: [B.ByteString] -> IO Int32
normalParse sx = do
  tx <- mapM (\(B.BS ptr len) -> withForeignPtr ptr $ \p -> processLineNormal p len) sx
  return $ L.foldl' (\s r -> s + _value r) 0 tx

buildInParse :: [B.ByteString] -> Int32
buildInParse sx = let tx = L.map Lib.processLineBuildIn sx
                  in  L.foldl' (\s r -> s + _value r) 0 tx

main :: IO ()
main = do
  -- let s = "Manila Long Long Long;14.5958\notherthing;10.12"
  -- tx <- case s of
  --         (B.BS ptr len) -> withForeignPtr ptr $ \p -> do
  --           r1 <- processLineSwar p len
  --           r2 <- processLineNormal p len
  --           return (r1, r2)
  -- print tx
  -- forM_ tempeLines (\(B.BS ptr len) -> do
  --                       r <- withForeignPtr ptr $ \p -> do
  --                         r1 <- processLineSwar p len
  --                         r2 <- processLineNormal p len
  --                         return (r1, r2)
  --   print r) 
  -- print (buildInParse tempeLines)
  defaultMain [
    bgroup "process 10 short lines(<16bytes)" [bench "SWAR parse " $ nfIO (swarParse tempeLines),
                                               bench "Ptr parse " $ nfIO (normalParse tempeLines),
                                               bench "Basic parse" $ whnf buildInParse tempeLines
                                              ],
    bgroup "process 10 long lines(=128bytes)" [bench "SWAR parse " $ nfIO (swarParse tempeLines2),
                                               bench "Ptr parse " $ nfIO (normalParse tempeLines2),
                                               bench "Basic parse" $ whnf buildInParse tempeLines2
                                              ]
    ]


