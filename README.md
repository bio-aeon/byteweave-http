## [WIP] ByteWeave HTTP
An experimental simple Idris 2 interface for HTTP services.

```idris
import Control.App

import ByteWeave.HTTP.Server
import ByteWeave.HTTP.InetSocketAddress

main : IO ()
main = run $ server "127.0.0.1:8000"
```
