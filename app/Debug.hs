{-# LANGUAGE OverloadedStrings #-}

module Debug where

import DNS.Header (DNSHeader, parseHeader)
import DNS.Packet (DNSPacket (..), getData, parseDNSPacket)
import DNS.Parser (DNSParseError)
import DNS.Question (DNSQuestion, parseQuestion)
import DNS.Record (DNSRecord, parseRecord)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll)
import Text.Megaparsec qualified as M

-- Debugging functions

_debugBuilderOutput :: Builder -> B.ByteString
_debugBuilderOutput = B16.encode . B.toStrict . toLazyByteString

_checkDNSQuery :: Socket -> IO ()
_checkDNSQuery =
  flip sendAll $ B16.decodeLenient "82980100000100000000000003777777076578616d706c6503636f6d0000010001"

_exampleResponse :: B.ByteString
_exampleResponse = B16.decodeLenient "60568180000100010000000003777777076578616d706c6503636f6d0000010001c00c000100010000529b00045db8d822"

-- >>> _checkDNSHeader
-- Right (DNSHeader {hid = 24662, hflags = 33152, hnumQuestions = 1, hnumAnswers = 1, hnumAuthorities = 0, hnumAdditionals = 0})
_checkDNSHeader :: Either (M.ParseErrorBundle B.ByteString Void) DNSHeader
_checkDNSHeader = M.parse parseHeader "" _exampleResponse

-- >>> _checkDNSQuestion
-- Right (DNSQuestion {qname = "www.example.com", qtype = 1, qclass = 1})
_checkDNSQuestion :: Either (M.ParseErrorBundle B.ByteString Void) DNSQuestion
_checkDNSQuestion = snd <$> M.parse _parseDNSHeaderAndQuestion "" _exampleResponse

_parseDNSHeaderAndQuestion :: M.Parsec Void B.ByteString (DNSHeader, DNSQuestion)
_parseDNSHeaderAndQuestion = do
  header <- parseHeader
  question <- parseQuestion
  pure (header, question)

-- >>> _parseDNSResponse
-- Right (DNSHeader {hid = 24662, hflags = 33152, hnumQuestions = 1, hnumAnswers = 1, hnumAuthorities = 0, hnumAdditionals = 0},DNSQuestion {qname = "www.example.com", qtype = 1, qclass = 1},DNSRecord {rname = "www.example.com", rtype = 1, rclass = 1, rttl = 21147, rdataLength = 4, rdata = "]\184\216\""})
_parseDNSResponse ::
  Either
    (M.ParseErrorBundle B.ByteString Void)
    (DNSHeader, DNSQuestion, DNSRecord)
_parseDNSResponse = M.parse parseDNSResponse "" _exampleResponse

-- >>> _parseFullDNSPacket
-- Right (DNSPacket {pheader = DNSHeader {hid = 24662, hflags = 33152, hnumQuestions = 1, hnumAnswers = 1, hnumAuthorities = 0, hnumAdditionals = 0}, pquestions = [DNSQuestion {qname = "www.example.com", qtype = 1, qclass = 1}], panswers = [DNSRecord {rname = "www.example.com", rtype = 1, rclass = 1, rttl = 21147, rdataLength = 4, rdata = "]\184\216\""}], pauthorities = [], padditionals = []})
_parseFullDNSPacket :: Either (M.ParseErrorBundle B.ByteString Void) DNSPacket
_parseFullDNSPacket = M.parse parseDNSPacket "" _exampleResponse

-- >>> _getData
-- Right ["]\184\216\""]
_getData :: Either (M.ParseErrorBundle B.ByteString Void) [B.ByteString]
_getData = getData <$> _parseFullDNSPacket

parseDNSResponse :: M.Parsec Void B.ByteString (DNSHeader, DNSQuestion, DNSRecord)
parseDNSResponse = do
  allInput <- M.getInput
  header <- parseHeader
  question <- parseQuestion
  record <- parseRecord allInput
  pure (header, question, record)

_getAuthorities :: Either DNSParseError DNSPacket -> Either DNSParseError [DNSRecord]
_getAuthorities packet = packet >>= Right <$> pauthorities

_getAnswers :: Either DNSParseError DNSPacket -> Either DNSParseError [DNSRecord]
_getAnswers packet = packet >>= Right <$> panswers

_getAdditionals :: Either DNSParseError DNSPacket -> Either DNSParseError [DNSRecord]
_getAdditionals packet = packet >>= Right <$> padditionals

_printQueryResult :: Either DNSParseError DNSPacket -> IO ()
_printQueryResult = T.putStrLn . T.pack . show
