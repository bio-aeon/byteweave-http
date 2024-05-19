module ByteWeave.InetSocketAddress

import public Data.Maybe
import public Data.String
import public Data.String.Extra
import public Network.Socket

public export
InetSocketAddress : Type
InetSocketAddress = (SocketAddress, Int)

public export
inetSocketAddressFromString : String -> Maybe InetSocketAddress
inetSocketAddressFromString str =
  case break (== ':') str of
       (_, "") => Nothing
       (addr, port) => do
         port' <- parseInteger (drop 1 port)
         Just (Hostname addr, port')

public export
fromString : 
  (str : String) -> 
  {auto prf : (IsJust (inetSocketAddressFromString str))} ->  
  InetSocketAddress
fromString str {prf} with (inetSocketAddressFromString str)
  fromString str {prf = ItIsJust} | Just addr = addr
