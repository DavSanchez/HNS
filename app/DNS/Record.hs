{-# LANGUAGE DerivingStrategies #-}

module DNS.Record (DNSRecord (..), parseRecord) where

import Control.Monad (replicateM)
import DNS.Parser (DNSParser, decodeDNSName)
import Data.ByteString qualified as B
import Data.Word (Word16, Word32)
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

parseRecord :: B.ByteString -> DNSParser DNSRecord
parseRecord fullInput = do
  name <- decodeDNSName fullInput
  rtype <- M.word16be
  rclass <- M.word16be
  rttl <- M.word32be
  rdataLength <- M.word16be
  rdata <- B.pack <$> replicateM (fromIntegral rdataLength) M.word8
  pure $ DNSRecord name rtype rclass rttl rdataLength rdata
