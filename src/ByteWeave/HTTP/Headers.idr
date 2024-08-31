module ByteWeave.HTTP.Headers

import public ByteWeave.HTTP.Header
import Data.String.Parser

public export
Headers : Type
Headers = List Header

public export
headersParser : Parser Headers
headersParser =
  some (headerParser <* string "\r\n")
