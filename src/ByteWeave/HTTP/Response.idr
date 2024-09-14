module ByteWeave.HTTP.Response

import Data.Buffer
import Language.JSON
import ByteWeave.HTTP.HTTPVersion
import ByteWeave.HTTP.Headers

data Status : Nat -> String -> Type where
  Ok : Status 200 "OK"

public export
record Response where
  constructor MkResponse
  status : Status code reason
  httpVersion : (mjr ** mnr ** HTTPVersion mjr mnr)
  headers : Headers
  body : String

public export
interface Encode a where
    encode : a -> String

export
Encode JSON where
    encode = show

export
Encode String where
    encode = id

export
ok : Encode a => a -> Response
ok val = 
  let content := encode val
      content_length := stringByteLength content
   in MkResponse Ok (1 ** 1 ** HTTP11) [("Content-Length", show content_length)] (content)

