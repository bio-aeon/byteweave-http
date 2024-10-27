module ByteWeave.HTTP.Uri

import Data.String.Parser
import Data.String

public export
Query : Type
Query = List (String, String)

public export
record Uri where
  constructor MkUri
  path : String
  query : Query

export
Eq Uri where
  x == y = x.path == y.path && x.query == y.query

export
Show Uri where
  show uri =
    let queryStr = case uri.query of
                     [] => ""
                     query =>
                       let segments = intersperse "&" $ map (\(k, v) => "\{k}=\{v}") query
                         in "?" ++ (concat $ segments)
                   in uri.path ++ queryStr

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
