{-# language BangPatterns #-}
{-# language NamedFieldPuns #-}

module HaProxy.Syslog
  ( Message(..)
  , decode
  ) where

import Data.Bytes (Bytes)
import Net.IPv4 (IPv4)
import Data.Bytes.Parser (Parser)
import Data.Word (Word16,Word64)

import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Net.IPv4 as IPv4

-- The HAProxy documentation at
-- documents the fields in logs:
--
-- Example Log:
-- Feb  6 12:14:14 localhost␣
--   haproxy[14389]: 10.0.1.2:33317 [06/Feb/2009:12:14:14.655] http-in␣
--   static/srv1 10/0/30/69/109 200 2750 - - ---- 1/1/1/1/0 0/0 {1wt.eu}␣
--   {} "GET /index.html HTTP/1.1"
--
-- Field   Format                                Extract from the example above
--     1   process_name '[' pid ']:'                            haproxy[14389]:
--     2   client_ip ':' client_port                             10.0.1.2:33317
--     3   '[' request_date ']'                      [06/Feb/2009:12:14:14.655]
--     4   frontend_name                                                http-in
--     5   backend_name '/' server_name                             static/srv1
--     6   TR '/' Tw '/' Tc '/' Tr '/' Ta*                       10/0/30/69/109
--     7   status_code                                                      200
--     8   bytes_read*                                                     2750
--     9   captured_request_cookie                                            -
--    10   captured_response_cookie                                           -
--    11   termination_state                                               ----
--    12   actconn '/' feconn '/' beconn '/' srv_conn '/' retries*    1/1/1/1/0
--    13   srv_queue '/' backend_queue                                      0/0
--    14   '{' captured_request_headers* '}'                   {haproxy.1wt.eu}
--    15   '{' captured_response_headers* '}'                                {}
--    16   '"' http_request '"'                      "GET /index.html HTTP/1.1"
--
-- In practice, the captured response headers appear to be optional. That is,
-- when the set is empty, the {} is simply omitted. Also, when haproxy is
-- performing TLS termination, I see the cipher and version at the end of
-- the log (e.g. ECDHE-RSA-AES256-GCM-SHA384 TLSv1.2). For example:
--
-- <134>Aug  5 14:03:21 my-server haproxy[121586]: 192.0.2.176:62109␣
--   [05/Aug/2020:14:03:21.707] my_frontend_https~ static/www-cache-1␣
--   122/0/1/2/125 200 1197 - - ---- 1717/1712/4/1/0 0/0␣
--   "GET /path/to/styles.css?foo HTTP/1.1" ECDHE-RSA-AES256-GCM-SHA384 TLSv1.2

data Message = Message
  { clientIp :: !IPv4
  , clientPort :: !Word16
  , frontendName :: {-# UNPACK #-} !Bytes
  , backendName :: {-# UNPACK #-} !Bytes
  , serverName :: {-# UNPACK #-} !Bytes
  , statusCode :: {-# UNPACK #-} !Word64
  , bytesRead :: {-# UNPACK #-} !Word64
  , method :: {-# UNPACK #-} !Bytes
  , path :: {-# UNPACK #-} !Bytes
  }

-- | Decode an HAProxy syslog message. This begins after the BSD-syslog
-- preamble. That is, the first thing in the log should be the client IP
-- address.
decode :: Bytes -> Maybe Message
decode = Parser.parseBytesMaybe parser

parser :: Parser () s Message
parser = do
  clientIp <- IPv4.parserUtf8Bytes ()
  Latin.char () ':'
  clientPort <- Latin.decWord16 ()
  Latin.char () ' '
  Latin.char () '['
  Latin.skipTrailedBy () ']'
  Latin.char () ' '
  frontendName <- Parser.takeTrailedBy () 0x20 -- space
  backendName <- Parser.takeTrailedBy () 0x2F -- slash
  serverName <- Parser.takeTrailedBy () 0x20 -- space
  Latin.skipTrailedBy () ' ' 
  statusCode <- Latin.decWord64 ()
  Latin.char () ' '
  bytesRead <- Latin.decWord64 ()
  Latin.char () ' '
  Latin.skipTrailedBy () ' ' 
  Latin.skipTrailedBy () ' ' 
  Latin.skipTrailedBy () ' ' 
  Latin.skipTrailedBy () ' ' 
  Latin.skipTrailedBy () ' ' 
  Latin.char () '"'
  method <- Parser.takeTrailedBy () 0x20 -- space
  path <- Parser.takeTrailedBy () 0x20 -- space
  Latin.skipTrailedBy () '"' 
  pure Message{clientIp,clientPort,frontendName,backendName,serverName,statusCode,bytesRead,method,path}
