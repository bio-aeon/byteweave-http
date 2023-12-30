module ByteWeave.Server

import Control.App
import Control.App.Console
import Network.Socket
import ByteWeave.InetSocketAddress

netApp : Socket -> SocketAddress -> (PrimIO es => App es ())
netApp sock addr = do
    Right (msg, _) <- primIO $ recv sock 1024
    | Left err => putStrLn $ "Error while receiving from the socket: " ++ show err
    Right _ <- primIO $ send sock msg
    | Left err => putStrLn $ "Error while sending to the socket: " ++ show err
    primIO $ close sock

serve : Socket -> (PrimIO es => App es ())
serve sock = do
    Right (sock', addr) <- primIO $ accept sock
    | Left err => putStrLn ("Error while accepting the socket: " ++ show err)
    netApp sock' addr
    serve sock

export
server : InetSocketAddress -> (PrimIO es => App es ())
server (addr, port) = do
    Right sock <- primIO $ socket AF_INET Stream 0
    | Left err => putStrLn $ "Error while creating a socket on port" ++ show port ++ ": " ++ show err
    0 <- primIO $ bind sock (Just addr) port
    | errno => do
        putStrLn $ "Error while binding the socket: errno=" ++ show errno
        primIO $ close sock 
    0 <- primIO $ listen sock
    | errno => do
        putStrLn $ "Error while listening on the socket: errno=" ++ show errno
        primIO $ close sock
    putStrLn $ "Server bound to address: " ++ show addr ++ ":" ++ show port
    serve sock
    primIO $ close sock
