{-# LANGUAGE OverloadedStrings #-}

module Debug where

import DNS.Header (DNSHeader, parseHeader)
import DNS.Packet (DNSPacket, parseDNSPacket)
import DNS.Question (DNSQuestion, parseQuestion)
import DNS.Record (DNSRecord, parseRecord)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Builder (Builder, toLazyByteString)
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
_exampleResponse = B16.decodeLenient "e35d8180000100010000000003777777076578616d706c6503636f6d0000010001c00c000100010000508900045db8d822"

-- >>> _checkDNSHeader
-- Right (DNSHeader {hid = 58205, hflags = 33152, hnumQuestions = 1, hnumAnswers = 1, hnumAuthorities = 0, hnumAdditionals = 0})
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
-- Left (ParseErrorBundle {bundleErrors = TrivialError 49 (Just EndOfInput) (fromList [Label ('8' :| " bit word")]) :| [], bundlePosState = PosState {pstateInput = "\227]\129\128\NUL\SOH\NUL\SOH\NUL\NUL\NUL\NUL\ETXwww\aexample\ETXcom\NUL\NUL\SOH\NUL\SOH\192\f\NUL\SOH\NUL\SOH\NUL\NULP\137\NUL\EOT]\184\216\"", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
_parseDNSResponse ::
  Either
    (M.ParseErrorBundle B.ByteString Void)
    (DNSHeader, DNSQuestion, DNSRecord)
_parseDNSResponse = M.parse parseDNSResponse "" _exampleResponse

-- >>> _parseFullDNSPacket
-- Left (ParseErrorBundle {bundleErrors = TrivialError 49 (Just EndOfInput) (fromList [Label ('8' :| " bit word")]) :| [], bundlePosState = PosState {pstateInput = "\227]\129\128\NUL\SOH\NUL\SOH\NUL\NUL\NUL\NUL\ETXwww\aexample\ETXcom\NUL\NUL\SOH\NUL\SOH\192\f\NUL\SOH\NUL\SOH\NUL\NULP\137\NUL\EOT]\184\216\"", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
_parseFullDNSPacket :: Either (M.ParseErrorBundle B.ByteString Void) DNSPacket
_parseFullDNSPacket = M.parse parseDNSPacket "" _exampleResponse

parseDNSResponse :: M.Parsec Void B.ByteString (DNSHeader, DNSQuestion, DNSRecord)
parseDNSResponse = do
  header <- parseHeader
  question <- parseQuestion
  record <- parseRecord
  pure (header, question, record)
