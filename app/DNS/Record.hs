{-# LANGUAGE DerivingStrategies #-}

module DNS.Record (DNSRecord, parseRecord) where

import DNS.Parser (decodeDNSName, getWord16)
import Data.Attoparsec.ByteString qualified as A
import Data.Bits (Bits ((.|.)), shiftL)
import Data.ByteString qualified as B
import Data.Word (Word16, Word32)

data DNSRecord = DNSRecord
  { rname :: B.ByteString,
    rtype :: !Word16,
    rclass :: !Word16,
    rttl :: !Word32,
    rdataLength :: !Word16,
    rdata :: B.ByteString
  }
  deriving stock (Show)

parseRecord :: A.Parser DNSRecord
parseRecord = do
  name <- decodeDNSName
  rtype <- getWord16
  rclass <- getWord16
  rttl <- getWord32
  rdataLength <- getWord16
  rdata <- A.take (fromIntegral rdataLength)
  pure $ DNSRecord name rtype rclass rttl rdataLength rdata
  where
    getWord32 :: A.Parser Word32
    getWord32 = getWord16 >>= \w1 -> getWord16 >>= \w2 -> pure $ combine w1 w2
      where
        combine :: Word16 -> Word16 -> Word32
        combine x y = (fromIntegral x `shiftL` 16) .|. fromIntegral y
