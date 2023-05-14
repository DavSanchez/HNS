{-# LANGUAGE DerivingStrategies #-}

module DNS.Record (DNSRecord, parseRecord) where

import Control.Monad (replicateM)
import DNS.Parser (decodeDNSName)
import Data.ByteString qualified as B
import Data.Void (Void)
import Data.Word (Word16, Word32)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte.Binary qualified as M

data DNSRecord = DNSRecord
  { rname :: B.ByteString,
    rtype :: !Word16,
    rclass :: !Word16,
    rttl :: !Word32,
    rdataLength :: !Word16,
    rdata :: B.ByteString
  }
  deriving stock (Show)

parseRecord :: M.Parsec Void B.ByteString DNSRecord
parseRecord = do
  name <- decodeDNSName
  rtype <- M.word16be
  rclass <- M.word16be
  rttl <- M.word32be
  rdataLength <- M.word16be
  rdata <- B.pack <$> replicateM (fromIntegral rdataLength) M.word8
  pure $ DNSRecord name rtype rclass rttl rdataLength rdata
