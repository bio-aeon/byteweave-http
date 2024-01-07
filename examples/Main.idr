module Main

import Control.App
import ByteWeave.Server
import ByteWeave.InetSocketAddress

main : IO ()
main = run $ server "127.0.0.1:8000"
