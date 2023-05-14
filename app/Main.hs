{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Arrow ((>>>))
import DNS.Query (buildQuery)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Word (Word16)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    SocketType (Datagram),
    connect,
    defaultProtocol,
    getAddrInfo,
    socket,
  )
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = do
  adrr : _ <- getAddrInfo Nothing (Just "8.8.8.8") (Just "53")
  sock <- socket (addrFamily adrr) Datagram defaultProtocol
  connect sock (addrAddress adrr)
  buildQuery "www.example.com" typeA >>= (toLazyByteString >>> B.toStrict >>> sendAll sock)
  -- _checkDNSQuery sock -- Check sample query
  response <- recv sock 1024
  (C8.putStrLn . B16.encode) response
  where
    typeA :: Word16
    typeA = 0x0001
