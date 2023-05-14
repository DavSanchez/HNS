{-# LANGUAGE DerivingStrategies #-}

module DNS.Packet (DNSPacket, parseDNSPacket) where

import Control.Monad (replicateM)
import DNS.Header (DNSHeader (..), parseHeader)
import DNS.Question (DNSQuestion, parseQuestion)
import DNS.Record (DNSRecord, parseRecord)
import Data.Attoparsec.ByteString qualified as A

data DNSPacket = DNSPacket
  { pheader :: DNSHeader,
    pquestions :: [DNSQuestion],
    panswers :: [DNSRecord],
    pauthorities :: [DNSRecord],
    padditionals :: [DNSRecord]
  }
  deriving stock (Show)

parseDNSPacket :: A.Parser DNSPacket
parseDNSPacket = do
  header <- parseHeader
  questions <- replicateM (fromIntegral $ hnumQuestions header) parseQuestion
  answers <- replicateM (fromIntegral $ hnumAnswers header) parseRecord
  authorities <- replicateM (fromIntegral $ hnumAuthorities header) parseRecord
  additionals <- replicateM (fromIntegral $ hnumAdditionals header) parseRecord
  pure $ DNSPacket header questions answers authorities additionals