{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Arrow ((>>>))
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString, word16BE, word8)
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy.Char8 qualified as C8
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

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  adrr : _ <- getAddrInfo Nothing (Just "8.8.8.8") (Just "53")
  sock <- socket (addrFamily adrr) Datagram defaultProtocol
  connect sock (addrAddress adrr)
  sendAll sock $ BS.toStrict . toLazyByteString $ buildQuery "www.example.com" typeA
  -- _checkDNSQuery sock
  response <- recv sock 1024
  (C8.putStrLn . C8.fromStrict . B16.encode) response

_checkDNSQuery :: Socket -> IO ()
_checkDNSQuery =
  flip sendAll $ B16.decodeLenient "82980100000100000000000003777777076578616d706c6503636f6d0000010001"

data Header = Header
  { hId :: Word16,
    flags :: Word16,
    numQuestions :: Word16,
    numAnswers :: Word16,
    numAuthorities :: Word16,
    numAdditionals :: Word16
  }

data Question = Question
  { qname :: BS.ByteString,
    qtype :: Word16,
    qclass :: Word16
  }

typeA :: Word16
typeA = 0x0001

classIn :: Word16
classIn = 0x0001

-- >>> B16.encode $ BS.toStrict $ toLazyByteString $  header2Bytes (Header 1 2 3 4 5 6)
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
  lazyByteString (qname q)
    <> word16BE (qtype q)
    <> word16BE (qclass q)

-- >>> _debugBuilderOutput $ encodeDNSName "www.example.com"
-- "03777777076578616d706c6503636f6d00"
encodeDNSName :: String -> Builder
encodeDNSName =
  let buildWord :: BS.ByteString -> Builder
      buildWord w = (word8 . fromIntegral . BS.length) w <> lazyByteString w
   in C8.pack >>> C8.split '.' >>> fmap buildWord >>> mconcat >>> (<> word8 0)

-- >>> _debugBuilderOutput $ buildQuery "www.example.com" typeA
-- "82980100000100000000000003777777076578616d706c6503636f6d0000010001"
--
-- "82980100000100000000000003777777076578616d706c6503636f6d0000010001"
buildQuery :: String -> Word16 -> Builder
buildQuery domainName recordType =
  let name = encodeDNSName domainName
      -- (qId, _) = genWord16 $ mkStdGen 1234 -- This obviously should be a true random number generator and not this.
      qId = 0x8298
      recursionDesired = 0x0100 -- 1 << 8
      header = Header qId recursionDesired 1 0 0 0
      question = Question (toLazyByteString name) recordType classIn
   in header2Bytes header <> question2Bytes question

_debugBuilderOutput :: Builder -> B.ByteString
_debugBuilderOutput = B16.encode . BS.toStrict . toLazyByteString
