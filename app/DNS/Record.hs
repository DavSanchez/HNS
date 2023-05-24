{-# LANGUAGE DerivingStrategies #-}

module DNS.Record (DNSRecord (..), parseRecord) where

import Control.Monad (replicateM)
import DNS.Parser (DNSParser, decodeDNSName)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.List (intersperse)
import Data.Word (Word16, Word32, Word8)
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
  rdata <- case rtype of
    0x0002 -> decodeDNSName fullInput
    0x0001 -> ip2ByteString <$> replicateM (fromIntegral rdataLength) M.word8
    _ -> B.pack <$> replicateM (fromIntegral rdataLength) M.word8
  pure $ DNSRecord name rtype rclass rttl rdataLength rdata

-- >>> ip2ByteString [0x11, 0x02, 0x03, 0x04]
-- "17.2.3.4"
ip2ByteString :: [Word8] -> C8.ByteString
ip2ByteString =
  C8.pack
    . mconcat
    . intersperse "."
    . map (show . (fromIntegral :: Word8 -> Int))
