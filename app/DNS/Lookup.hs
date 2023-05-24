module DNS.Lookup (lookupDomain, sendQuery) where

import Control.Arrow ((>>>))
import DNS.IP4 (getIPs)
import DNS.Packet (getData, parseDNSPacket)
import DNS.Parser (DNSParseResult)
import DNS.Query (buildQuery)
import Data.ByteString qualified as B
import Data.ByteString.Builder (toLazyByteString)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Word (Word16)
import Network.Socket (AddrInfo (addrAddress, addrFamily), HostName, SocketType (Datagram), connect, defaultProtocol, getAddrInfo, socket)
import Network.Socket.ByteString (recv, sendAll)
import Text.Megaparsec qualified as M

lookupDomain :: String -> IO (DNSParseResult [T.Text])
lookupDomain domain = do
  let query = buildQuery domain typeA
  adrr : _ <- getAddrInfo Nothing (Just "8.8.8.8") (Just "53")
  sock <- socket (addrFamily adrr) Datagram defaultProtocol
  connect sock (addrAddress adrr)
  query >>= (toLazyByteString >>> B.toStrict >>> sendAll sock)
  packet <- M.runParser parseDNSPacket mempty <$> recv sock 1024
  let res = getData <$> packet
  pure (res >>= traverse getIPs)
  where
    typeA :: Word16
    typeA = 0x0001

sendQuery :: HostName -> String -> Word16 -> IO ()
sendQuery ip domain recordType = do
  let query = buildQuery domain recordType
  adrr : _ <- getAddrInfo Nothing (Just ip) (Just "53")
  sock <- socket (addrFamily adrr) Datagram defaultProtocol
  connect sock (addrAddress adrr)
  query >>= (toLazyByteString >>> B.toStrict >>> sendAll sock)
  packet <- M.runParser parseDNSPacket mempty <$> recv sock 1024
  (T.putStrLn . T.pack . show) packet
