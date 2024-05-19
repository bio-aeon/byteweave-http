module ByteWeave.Request

import Data.String.Parser
import Data.String
import ByteWeave.Method
import ByteWeave.HttpVersion
import ByteWeave.Headers

public export
record Request where
  constructor MkRequest
  method : Method
  uri : String
  httpVersion : (mjr ** mnr ** HttpVersion mjr mnr)
  headers : Headers
  body : String

export
Show Request where
  show (MkRequest method uri (mjr ** mnr ** version) headers body) = """
    \{show method} \{uri} \{show version}
    \{concat $ intersperse "\n" $ map (\(hd, con) => "\{hd}: \{con}") headers}
    \{body}
    """

requestLineParser : Parser (Method, String, (mjr ** mnr ** HttpVersion mjr mnr))
requestLineParser =
  (,,) <$> methodParser <* spaces1 <*> takeWhile (/= ' ') <* spaces1 <* string "HTTP/"
       <*> httpVersionParser <* string "\r\n"

public export
requestParser : Parser Request
requestParser =
  (\(m, u, v) => \h => \b => MkRequest m u v h b)
    <$> requestLineParser <*> headersParser <*> takeWhile (\_ => True) <* eos

public export
decodeRequest : String -> Either String Request
decodeRequest str = fst <$> (parse requestParser str)
