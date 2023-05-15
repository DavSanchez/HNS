module DNS.IP4 (getIPs, parseIP4) where

import DNS.Parser (DNSParseResult, DNSParser)
import Data.ByteString qualified as B
import Data.List (intercalate)
import Data.Text qualified as T
import Data.Word (Word8)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Byte.Binary qualified as M

type IP4 = (Word8, Word8, Word8, Word8)

parseIP4 :: DNSParser IP4
parseIP4 = do
  a <- M.word8
  b <- M.word8
  c <- M.word8
  d <- M.word8
  pure (a, b, c, d)

-- >>> :set -XOverloadedStrings
-- >>> printIP4 (192, 168, 1, 1)
-- "192.168.1.1"
printIP4 :: IP4 -> T.Text
printIP4 ip = T.pack $ intercalate ['.'] $ show <$> [a, b, c, d]
  where
    (a, b, c, d) = ip

-- >>> :set -XOverloadedStrings
-- >>> getIPs "]\184\216\""
-- Right "93.184.216.34"
getIPs :: B.ByteString -> DNSParseResult T.Text
getIPs i = printIP4 <$> M.runParser parseIP4 "IP4" i
