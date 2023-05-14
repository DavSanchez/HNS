{-# LANGUAGE OverloadedStrings #-}

module Debug where

import DNS.Header (DNSHeader, parseHeader)
import DNS.Packet (DNSPacket, parseDNSPacket)
import DNS.Question (DNSQuestion, parseQuestion)
import DNS.Record (DNSRecord, parseRecord)
import Data.Attoparsec.ByteString qualified as A
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Builder (Builder, toLazyByteString)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll)

-- Debugging functions

_debugBuilderOutput :: Builder -> B.ByteString
_debugBuilderOutput = B16.encode . B.toStrict . toLazyByteString

_checkDNSQuery :: Socket -> IO ()
_checkDNSQuery =
  flip sendAll $ B16.decodeLenient "82980100000100000000000003777777076578616d706c6503636f6d0000010001"

_exampleResponse :: B.ByteString
_exampleResponse = B16.decodeLenient "e35d8180000100010000000003777777076578616d706c6503636f6d0000010001c00c000100010000508900045db8d822"

-- >>> _checkDNSHeader
-- Right (DNSHeader {hid = 58205, hflags = 33152, hnumQuestions = 1, hnumAnswers = 1, hnumAuthorities = 0, hnumAdditionals = 0})
_checkDNSHeader :: Either String DNSHeader
_checkDNSHeader = A.parseOnly parseHeader _exampleResponse

-- >>> _checkDNSQuestion
-- Right (DNSQuestion {qname = "www.example.com", qtype = 1, qclass = 1})
_checkDNSQuestion :: Either String DNSQuestion
_checkDNSQuestion = snd <$> A.eitherResult (A.parse _parseDNSHeaderAndQuestion _exampleResponse)

_parseDNSHeaderAndQuestion :: A.Parser (DNSHeader, DNSQuestion)
_parseDNSHeaderAndQuestion = do
  header <- parseHeader
  question <- parseQuestion
  pure (header, question)

-- >>> _parseDNSResponse
-- Left "not enough input"
_parseDNSResponse :: Either String (DNSHeader, DNSQuestion, DNSRecord)
_parseDNSResponse = A.parseOnly parseDNSResponse _exampleResponse

-- >>> _parseFullDNSPacket
-- Left "not enough input"
_parseFullDNSPacket :: Either String DNSPacket
_parseFullDNSPacket = A.parseOnly parseDNSPacket _exampleResponse

parseDNSResponse :: A.Parser (DNSHeader, DNSQuestion, DNSRecord)
parseDNSResponse = do
  header <- parseHeader
  question <- parseQuestion
  record <- parseRecord
  pure (header, question, record)
