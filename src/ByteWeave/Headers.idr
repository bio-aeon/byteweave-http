module ByteWeave.Headers

import public ByteWeave.Header
import Data.String.Parser

public export
Headers : Type
Headers = List Header

public export
headersParser : Parser Headers
headersParser =
  some (headerParser <* string "\r\n")
