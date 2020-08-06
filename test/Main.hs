{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}

import HaProxy.Syslog (decode,Message(Message))
import Data.Bytes (Bytes)

import qualified Data.Bytes as Bytes
import qualified HaProxy.Syslog
import qualified Net.IPv4 as IPv4

main :: IO ()
main = do
  putStrLn "Starting"
  putStrLn "Test A"
  case decode msgA of
    Nothing -> fail "Could not decode message A"
    Just (Message {serverName,clientIp,clientPort,method}) -> do
      assert "clientIp" (clientIp == IPv4.fromOctets 192 0 2 176)
      assert "clientPort" (clientPort == 62109)
      assert "serverName" (serverName == Bytes.fromLatinString "www-cache-1")
      assert "method" (method == Bytes.fromLatinString "GET")
  putStrLn "Finished"

assert :: String -> Bool -> IO ()
assert ctx b = if b then pure () else fail ctx

msgA :: Bytes
msgA = Bytes.fromLatinString
  "192.0.2.176:62109 [05/Aug/2020:14:03:21.707] my_frontend_https~ static/www-cache-1\
  \ 122/0/1/2/125 200 1197 - - ---- 1717/1712/4/1/0 0/0\
  \ \"GET /path/to/styles.css?foo HTTP/1.1\" ECDHE-RSA-AES256-GCM-SHA384 TLSv1.2"
