{-# LANGUAGE DerivingStrategies #-}

module DNS.Question (DNSQuestion (..), question2Bytes, parseQuestion) where

import DNS.Parser (DNSParser, decodeDNSNameSimple)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Builder (Builder, byteString, toLazyByteString, word16BE)
import Data.Word (Word16)
import Text.Megaparsec.Byte.Binary qualified as M

data DNSQuestion = DNSQuestion
  { qname :: B.ByteString,
    qtype :: !Word16,
    qclass :: !Word16
  }
  deriving stock (Show)

-- >>> _debugBuilderOutput $ question2Bytes (DNSQuestion "www.example.com" 1 1)
-- "7777772e6578616d706c652e636f6d00010001"
question2Bytes :: DNSQuestion -> Builder
question2Bytes q =
  byteString (qname q)
    <> word16BE (qtype q)
    <> word16BE (qclass q)

parseQuestion :: DNSParser DNSQuestion
parseQuestion =
  DNSQuestion
    <$> decodeDNSNameSimple
    <*> M.word16be
    <*> M.word16be

_debugBuilderOutput :: Builder -> B.ByteString
_debugBuilderOutput = B16.encode . B.toStrict . toLazyByteString
