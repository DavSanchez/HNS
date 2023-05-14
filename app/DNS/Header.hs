{-# LANGUAGE DerivingStrategies #-}

module DNS.Header (DNSHeader (..), parseHeader, header2Bytes) where

import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder, word16BE)
import Data.Void (Void)
import Data.Word (Word16)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte.Binary qualified as M

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
parseHeader :: M.Parsec Void B.ByteString DNSHeader
parseHeader =
  DNSHeader
    <$> M.word16be
    <*> M.word16be
    <*> M.word16be
    <*> M.word16be
    <*> M.word16be
    <*> M.word16be
