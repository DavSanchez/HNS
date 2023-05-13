{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Builder (Builder, byteString, toLazyByteString, word16BE, word8)
import Data.ByteString.Char8 qualified as C8
import Data.Word (Word16)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    Socket,
    SocketType (Datagram),
    connect,
    defaultProtocol,
    getAddrInfo,
    socket,
  )
import Network.Socket.ByteString (recv, sendAll)
import System.Random (genWord16, initStdGen)

main :: IO ()
main = do
  adrr : _ <- getAddrInfo Nothing (Just "8.8.8.8") (Just "53")
  sock <- socket (addrFamily adrr) Datagram defaultProtocol
  connect sock (addrAddress adrr)
  _ <- sendAll sock . B.toStrict . toLazyByteString <$> buildQuery "www.example.com" typeA
  -- _checkDNSQuery sock -- Check sample query
  response <- recv sock 1024
  (C8.putStrLn . B16.encode) response

data Header = Header
  { hId :: !Word16,
    flags :: !Word16,
    numQuestions :: !Word16,
    numAnswers :: !Word16,
    numAuthorities :: !Word16,
    numAdditionals :: !Word16
  }

data Question = Question
  { qname :: B.ByteString,
    qtype :: !Word16,
    qclass :: !Word16
  }

typeA :: Word16
typeA = 0x0001

classIn :: Word16
classIn = 0x0001

-- >>> _debugBuilderOutput $  header2Bytes (Header 1 2 3 4 5 6)
-- "000100020003000400050006"
header2Bytes :: Header -> Builder
header2Bytes h =
  word16BE (hId h)
    <> word16BE (flags h)
    <> word16BE (numQuestions h)
    <> word16BE (numAnswers h)
    <> word16BE (numAuthorities h)
    <> word16BE (numAdditionals h)

-- >>> _debugBuilderOutput $ question2Bytes (Question "www.example.com" 1 1)
-- "7777772e6578616d706c652e636f6d00010001"
question2Bytes :: Question -> Builder
question2Bytes q =
  byteString (qname q)
    <> word16BE (qtype q)
    <> word16BE (qclass q)

-- >>> _debugBuilderOutput $ encodeDNSName "www.example.com"
-- "03777777076578616d706c6503636f6d00"
encodeDNSName :: String -> Builder
encodeDNSName =
  let buildWord :: B.ByteString -> Builder
      buildWord w = (word8 . fromIntegral . B.length) w <> byteString w
   in C8.pack >>> C8.split '.' >>> fmap buildWord >>> mconcat >>> (<> word8 0)

-- >>> _debugBuilderOutput <$> buildQuery "www.example.com" typeA
-- "b83f0100000100000000000003777777076578616d706c6503636f6d0000010001"
buildQuery :: (MonadIO m) => String -> Word16 -> m Builder
buildQuery domainName recordType = do
  qId <- fst . genWord16 <$> initStdGen -- Random number
  let name = encodeDNSName domainName
      recursionDesired = 0x0100 -- 1 << 8
      header = Header qId recursionDesired 1 0 0 0
      question = Question (B.toStrict . toLazyByteString $ name) recordType classIn
  pure $ header2Bytes header <> question2Bytes question

_debugBuilderOutput :: Builder -> B.ByteString
_debugBuilderOutput = B16.encode . B.toStrict . toLazyByteString

_checkDNSQuery :: Socket -> IO ()
_checkDNSQuery =
  flip sendAll $ B16.decodeLenient "82980100000100000000000003777777076578616d706c6503636f6d0000010001"
