module ByteWeave.HTTP.Header

import Data.String.Parser

public export
Header : Type
Header = (String, String)

public export
headerParser : Parser Header
headerParser =
  (,) <$> takeWhile1 (/= ':') <* spaces <* string ":" <* spaces <*> takeWhile1 (/= '\r')
