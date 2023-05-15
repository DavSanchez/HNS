{-# LANGUAGE OverloadedStrings #-}

module DNS.Parser (decodeDNSName, decodeDNSNameSimple, DNSParser, DNSParseResult) where

import Control.Monad (replicateM)
import Data.Bits (Bits ((.&.), (.|.)), shiftL)
import Data.ByteString qualified as B
import Data.Void (Void)
import Data.Word (Word16, Word8)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte.Binary qualified as M

type DNSParser a = M.Parsec Void B.ByteString a

type DNSParseResult a = Either (M.ParseErrorBundle B.ByteString Void) a

decodeDNSNameSimple :: DNSParser B.ByteString
decodeDNSNameSimple = do
  len <- M.word8
  if len == 0
    then pure mempty
    else do
      name <- B.pack <$> replicateM (fromIntegral len) M.word8
      rest <- decodeDNSNameSimple
      pure $ name <> (if B.null rest then mempty else "." <> rest)

decodeDNSName :: B.ByteString -> DNSParser B.ByteString
decodeDNSName i = do
  len <- M.word8
  if len == 0
    then pure mempty
    else
      if (len .&. 0b1100_0000) == 0b1100_0000
        then decodeCompressedDNSName i len
        else do
          name <- B.pack <$> replicateM (fromIntegral len) M.word8
          rest <- decodeDNSName i
          pure $ name <> (if B.null rest then mempty else "." <> rest)

decodeCompressedDNSName :: B.ByteString -> Word8 -> DNSParser B.ByteString
decodeCompressedDNSName i l = do
  offset' <- fromIntegral <$> M.word8
  let bytes = ((fromIntegral l :: Word16) .&. 0b0011_1111) `shiftL` 8
      pointer = fromIntegral (bytes .|. offset')
  currentPos <- M.getOffset
  currentInput <- M.getInput
  M.setInput i
  M.skipCount pointer M.word8
  name <- decodeDNSName i
  M.setInput currentInput
  M.setOffset currentPos
  pure name
