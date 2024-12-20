module ByteWeave.HTTP.Server

import Control.App
import Control.App.Console
import Network.Socket

import ByteWeave.HTTP.InetSocketAddress
import ByteWeave.HTTP.Method
import ByteWeave.HTTP.Header
import ByteWeave.HTTP.Headers
import ByteWeave.HTTP.HTTPVersion
import ByteWeave.HTTP.Request
import ByteWeave.HTTP.Logging

netApp : Socket -> SocketAddress -> (PrimIO es => App es ())
netApp sock addr = do
  Right (rawRequest, _) <- primIO $ recv sock 1024
  | Left err => logStringStdout <& "Error while receiving from the socket: " ++ show err
  Right _ <- primIO $ send sock rawRequest
  | Left err => logStringStdout <& "Error while sending to the socket: " ++ show err
  primIO $ close sock

serve : Socket -> (PrimIO es => App es ())
serve sock = do
  Right (sock', addr) <- primIO $ accept sock
  | Left err => logStringStdout <& "Error while accepting the socket: " ++ show err
  netApp sock' addr
  serve sock

export
server : InetSocketAddress -> (PrimIO es => App es ())
server (addr, port) = do
  Right sock <- primIO $ socket AF_INET Stream 0
  | Left err => logStringStdout <& "Error while creating a socket on port" ++ show port ++ ": " ++ show err
  0 <- primIO $ bind sock (Just addr) port
  | errno => do
    logStringStdout <& "Error while binding the socket: errno=" ++ show errno
    primIO $ close sock 
  0 <- primIO $ listen sock
  | errno => do
    logStringStdout <& "Error while listening on the socket: errno=" ++ show errno
    primIO $ close sock
  logStringStdout <& "Server bound to address: " ++ show addr ++ ":" ++ show port
  serve sock
  primIO $ close sock
