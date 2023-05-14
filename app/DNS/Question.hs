{-# LANGUAGE DerivingStrategies #-}

module DNS.Question (DNSQuestion (..), question2Bytes, parseQuestion) where

import DNS.Parser (decodeDNSName, getWord16)
import Data.Attoparsec.ByteString qualified as A
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder, byteString, word16BE)
import Data.Word (Word16)

data DNSQuestion = DNSQuestion
  { qname :: B.ByteString,
    qtype :: !Word16,
    qclass :: !Word16
  }
  deriving stock (Show)

-- >>> _debugBuilderOutput $ question2Bytes (Question "www.example.com" 1 1)
-- "7777772e6578616d706c652e636f6d00010001"
question2Bytes :: DNSQuestion -> Builder
question2Bytes q =
  byteString (qname q)
    <> word16BE (qtype q)
    <> word16BE (qclass q)

parseQuestion :: A.Parser DNSQuestion
parseQuestion =
  DNSQuestion
    <$> decodeDNSName
    <*> getWord16
    <*> getWord16
