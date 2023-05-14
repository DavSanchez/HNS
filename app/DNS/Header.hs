{-# LANGUAGE DerivingStrategies #-}

module DNS.Header (DNSHeader (..), parseHeader, header2Bytes) where

import DNS.Parser (getWord16)
import Data.Attoparsec.ByteString qualified as A
import Data.ByteString.Builder (Builder, word16BE)
import Data.Word (Word16)

data DNSHeader = DNSHeader
  { hid :: !Word16,
    hflags :: !Word16,
    hnumQuestions :: !Word16,
    hnumAnswers :: !Word16,
    hnumAuthorities :: !Word16,
    hnumAdditionals :: !Word16
  }
  deriving stock (Show)

-- >>> _debugBuilderOutput $  header2Bytes (Header 1 2 3 4 5 6)
-- "000100020003000400050006"
header2Bytes :: DNSHeader -> Builder
header2Bytes h =
  word16BE (hid h)
    <> word16BE (hflags h)
    <> word16BE (hnumQuestions h)
    <> word16BE (hnumAnswers h)
    <> word16BE (hnumAuthorities h)
    <> word16BE (hnumAdditionals h)

-- >>>
parseHeader :: A.Parser DNSHeader
parseHeader =
  DNSHeader
    <$> getWord16
    <*> getWord16
    <*> getWord16
    <*> getWord16
    <*> getWord16
    <*> getWord16
