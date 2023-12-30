module ByteWeave.InetSocketAddress

import public Network.Socket

public export
InetSocketAddress : Type
InetSocketAddress = (SocketAddress, Int)
