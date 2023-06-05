{-# LANGUAGE DerivingStrategies #-}

module DNS.Packet (DNSPacket (..), parseDNSPacket, getData) where

import Control.Monad (replicateM)
import DNS.Header (DNSHeader (..), parseHeader)
import DNS.Parser (DNSParser)
import DNS.Question (DNSQuestion, parseQuestion)
import DNS.Record (DNSRecord (..), parseRecord)
import Data.ByteString qualified as B
import Text.Megaparsec qualified as M

data DNSPacket = DNSPacket
  { pheader :: DNSHeader,
    pquestions :: [DNSQuestion],
    panswers :: [DNSRecord],
    pauthorities :: [DNSRecord],
    padditionals :: [DNSRecord]
  }
  deriving stock (Show)

parseDNSPacket :: DNSParser DNSPacket
parseDNSPacket = do
  fullInput <- M.getInput
  let parseRecord' = parseRecord fullInput
  header <- parseHeader
  questions <- replicateM (fromIntegral $ hnumQuestions header) parseQuestion
  answers <- replicateM (fromIntegral $ hnumAnswers header) parseRecord'
  authorities <- replicateM (fromIntegral $ hnumAuthorities header) parseRecord'
  additionals <- replicateM (fromIntegral $ hnumAdditionals header) parseRecord'
  pure $ DNSPacket header questions answers authorities additionals

getData :: DNSPacket -> [B.ByteString]
getData p = rdata <$> panswers p
