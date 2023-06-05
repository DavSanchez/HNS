module DNS.Lookup (lookupDomain, sendQuery) where

import Control.Arrow ((>>>))
import DNS.IP4 (getIPs)
import DNS.Packet (DNSPacket, getData, parseDNSPacket)
import DNS.Parser (DNSParseError, DNSParseResult)
import DNS.Query (buildQuery)
import Data.ByteString qualified as B
import Data.ByteString.Builder (toLazyByteString)
import Data.Text qualified as T
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

-- >>> sendQuery "8.8.8.8" "www.example.com" 0x01
-- Right (DNSPacket {pheader = DNSHeader {hid = 36307, hflags = 32896, hnumQuestions = 1, hnumAnswers = 1, hnumAuthorities = 0, hnumAdditionals = 0}, pquestions = [DNSQuestion {qname = "www.example.com", qtype = 1, qclass = 1}], panswers = [DNSRecord {rname = "www.example.com", rtype = 1, rclass = 1, rttl = 21231, rdataLength = 4, rdata = "93.184.216.34"}], pauthorities = [], padditionals = []})
sendQuery :: HostName -> String -> Word16 -> IO (Either DNSParseError DNSPacket)
sendQuery ip domain recordType = do
  let query = buildQuery domain recordType
  adrr : _ <- getAddrInfo Nothing (Just ip) (Just "53")
  sock <- socket (addrFamily adrr) Datagram defaultProtocol
  connect sock (addrAddress adrr)
  query >>= (toLazyByteString >>> B.toStrict >>> sendAll sock)
  M.runParser parseDNSPacket mempty <$> recv sock 1024
