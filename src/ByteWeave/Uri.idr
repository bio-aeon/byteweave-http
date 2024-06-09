module ByteWeave.Uri

import Data.String.Parser

Query : Type
Query = List (String, String)

public export
record Uri where
  constructor MkUri
  path : String
  query : Query

pathEndSymbols : List Char
pathEndSymbols = [' ', '?', '#']

pathParser : Parser String
pathParser = takeWhile (\c => not (elem c pathEndSymbols))

segmentEndSymbols : List Char
segmentEndSymbols = [' ', '&', '#']

segmentParser : Parser (String, String)
segmentParser =
  (,) <$> takeWhile(/= '=') <* string "=" <*> takeWhile (\c => not (elem c segmentEndSymbols))

queryParser : Parser Query
queryParser = some (segmentParser <* takeWhile (== '&'))

export
uriParser : Parser Uri
uriParser =
  (\p, q => MkUri p q) <$> pathParser <*> (char '?' *> queryParser <|> pure [])
