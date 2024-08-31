module ByteWeave.HTTP.Method

import Data.String.Parser

public export
data Method = Get
            | Head
            | Post
            | Put
            | Patch
            | Delete
            | Options
            | Unknown String

export
Show Method where
  show Get = "GET"
  show Head = "HEAD"
  show Post = "POST"
  show Put = "PUT"
  show Patch = "PATCH"
  show Delete = "DELETE"
  show Options = "OPTIONS"
  show (Unknown str) = str

export
methodFromString : String -> Method
methodFromString str =
  case str of
       "GET" => Get
       "HEAD" => Head
       "POST" => Post
       "PUT" => Put
       "PATCH" => Patch
       "DELETE" => Delete
       "OPTIONS" => Options
       _ => Unknown str

export
methodParser : Parser Method
methodParser = methodFromString <$> takeWhile1 isAlphaNum
