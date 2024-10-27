module ByteWeave.HTTP.Headers

import Data.String.Parser

import public ByteWeave.HTTP.Header

public export
Headers : Type
Headers = List Header

public export
headersParser : Parser Headers
headersParser =
  some (headerParser <* string "\r\n")

public export
encodeHeaders : Headers -> String
encodeHeaders [] = ""
encodeHeaders ((k, v) :: xs) =
  k ++ ": " ++ v ++ "\r\n" ++ encodeHeaders xs
