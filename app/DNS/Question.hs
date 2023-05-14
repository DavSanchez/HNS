{-# LANGUAGE DerivingStrategies #-}

module DNS.Question (DNSQuestion (..), question2Bytes, parseQuestion) where

import DNS.Parser (decodeDNSName)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder, byteString, word16BE)
import Data.Void (Void)
import Data.Word (Word16)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte.Binary qualified as M

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

parseQuestion :: M.Parsec Void B.ByteString DNSQuestion
parseQuestion =
  DNSQuestion
    <$> decodeDNSName
    <*> M.word16be
    <*> M.word16be
