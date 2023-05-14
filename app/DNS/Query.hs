module DNS.Query (buildQuery) where

import Control.Arrow ((>>>))
import Control.Monad.IO.Class (MonadIO)
import DNS.Header (DNSHeader (..), header2Bytes)
import DNS.Question (DNSQuestion (..), question2Bytes)
import Data.Bits (shiftL)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder, byteString, toLazyByteString, word8)
import Data.ByteString.Char8 qualified as C8
import Data.Word (Word16)
import System.Random (genWord16, initStdGen)

classIn :: Word16
classIn = 0x0001

-- >>> _debugBuilderOutput $ encodeDNSName "www.example.com"
-- "03777777076578616d706c6503636f6d00"
encodeDNSName :: String -> Builder
encodeDNSName =
  let buildWord :: B.ByteString -> Builder
      buildWord w = (word8 . fromIntegral . B.length) w <> byteString w
   in C8.pack >>> C8.split '.' >>> fmap buildWord >>> mconcat >>> (<> word8 0)

-- >>> _debugBuilderOutput <$> buildQuery "www.example.com" typeA
-- "216a0100000100000000000003777777076578616d706c6503636f6d0000010001"
buildQuery :: (MonadIO m) => String -> Word16 -> m Builder
buildQuery domainName recordType = do
  qId <- fst . genWord16 <$> initStdGen -- Random number
  let name = encodeDNSName domainName
      recursionDesired = 1 `shiftL` 8
      header = DNSHeader qId recursionDesired 1 0 0 0
      question = DNSQuestion (B.toStrict . toLazyByteString $ name) recordType classIn
  pure $ header2Bytes header <> question2Bytes question
