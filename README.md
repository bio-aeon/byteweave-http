## [WIP] ByteWeave HTTP
An experimental simple Idris 2 interface for HTTP services.

```idris
import Control.App
import ByteWeave.Server
import ByteWeave.InetSocketAddress

main : IO ()
main = run $ server (Hostname "127.0.0.1", 8000)
```
