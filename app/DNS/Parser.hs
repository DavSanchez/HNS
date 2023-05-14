{-# LANGUAGE OverloadedStrings #-}

module DNS.Parser (decodeDNSName) where

import Control.Monad (replicateM)
import Data.Bits (Bits ((.&.), (.|.)), shiftL)
import Data.ByteString qualified as B
import Data.Void (Void)
import Data.Word (Word16, Word8)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte.Binary qualified as M

decodeDNSName :: M.Parsec Void B.ByteString B.ByteString
decodeDNSName = do
  len <- M.word8
  if len == 0
    then pure mempty
    else
      if (len .&. 0b1100_0000) == 0b1100_0000
        then decodeCompressedDNSName len
        else do
          name <- B.pack <$> replicateM (fromIntegral len) M.word8
          rest <- decodeDNSName
          pure $ name <> (if B.null rest then mempty else "." <> rest)

decodeCompressedDNSName :: Word8 -> M.Parsec Void B.ByteString B.ByteString
decodeCompressedDNSName l = do
  offset' <- M.word8
  let bytes = ((fromIntegral l :: Word16) .&. 0b0011_1111) `shiftL` 8
  let pointer = bytes .|. (fromIntegral offset' :: Word16)
  currentPos <- M.getOffset
  M.skipCount (fromIntegral pointer) M.word8
  result <- decodeDNSName
  M.setOffset currentPos
  pure result
