{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Builder (Builder, charUtf8, lazyByteString, stringUtf8, toLazyByteString, word16BE)
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Word (Word16)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  BS.putStrLn $ toLazyByteString $ buildQuery "www.google.com" typeA

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
typeA = 1

classIn :: Word16
classIn = 1

-- >>> header2Bytes (Header 1 2 3 4 5 6)
-- "\NUL\SOH\NUL\STX\NUL\ETX\NUL\EOT\NUL\ENQ\NUL\ACK"
header2Bytes :: Header -> Builder
header2Bytes h =
  word16BE (hId h)
    <> word16BE (flags h)
    <> word16BE (numQuestions h)
    <> word16BE (numAnswers h)
    <> word16BE (numAuthorities h)
    <> word16BE (numAdditionals h)

-- >>> question2Bytes (Question "www.google.com" 1 1)
-- "www.google.com\NUL\SOH\NUL\SOH"
question2Bytes :: Question -> Builder
question2Bytes q =
  lazyByteString (qname q)
    <> word16BE (qtype q)
    <> word16BE (qclass q)

-- >>> encodeDNSName "www.google.com"
-- "3www6google3com0"
encodeDNSName :: String -> Builder
encodeDNSName =
  let lenWord :: String -> [String]
      lenWord w = [show (length w), w]
      splitDots :: String -> [String]
      splitDots = words . map (\c -> if c == '.' then ' ' else c)
   in flip (<>) (charUtf8 '0') . stringUtf8 . mconcat . concatMap lenWord . splitDots

buildQuery :: String -> Word16 -> Builder
buildQuery domainName recordType =
  let name = encodeDNSName domainName
      qId = 1 -- FIXME should be a random number
      recursionDesired = 0x100 -- 1 << 8
      header = Header qId recursionDesired 1 0 0 0
      question = Question (toLazyByteString name) recordType classIn
   in header2Bytes header <> question2Bytes question
