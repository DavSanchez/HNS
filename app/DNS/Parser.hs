{-# LANGUAGE OverloadedStrings #-}

module DNS.Parser (getWord16, decodeDNSName) where

import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.Combinator qualified as A
import Data.Bits (Bits ((.&.), (.|.)), shiftL)
import Data.ByteString qualified as B
import Data.Word (Word16, Word8)

getWord16 :: A.Parser Word16
getWord16 = A.anyWord8 >>= \w1 -> A.anyWord8 >>= \w2 -> pure $ combine w1 w2
  where
    combine :: Word8 -> Word8 -> Word16
    combine x y = (fromIntegral x `shiftL` 8) .|. fromIntegral y

decodeDNSName :: A.Parser B.ByteString
decodeDNSName = do
  len <- A.anyWord8
  if len == 0
    then pure mempty
    else
      if len .&. 0b1100_0000 == 0b1100_0000
        then decodeCompressedDNSName len
        else do
          name <- A.take (fromIntegral len)
          rest <- decodeDNSName
          pure $ name <> (if B.null rest then mempty else "." <> rest)

decodeCompressedDNSName :: Word8 -> A.Parser B.ByteString
decodeCompressedDNSName l = do
  offset' <- fromIntegral <$> A.anyWord8
  let bytes = (fromIntegral l .&. 0b0011_1111) `shiftL` 8
  let pointer = bytes .|. offset' :: Int
  A.lookAhead $ A.take pointer >> decodeDNSName
