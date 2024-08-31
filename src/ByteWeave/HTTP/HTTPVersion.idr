module ByteWeave.HTTP.HTTPVersion

import Data.String.Parser

public export
data HTTPVersion : Nat -> Nat -> Type where
  HTTP09 : HTTPVersion 0 9
  HTTP10 : HTTPVersion 1 0
  HTTP11 : HTTPVersion 1 1
  HTTP20 : HTTPVersion 2 0
  HTTP30 : HTTPVersion 3 0

export
{mjr, mnr : _} -> Show (HTTPVersion mjr mnr) where
  show {mjr} {mnr} version =
    let versionStr = if mjr >= 2
                       then show mjr
                       else "\{show mjr}.\{show mnr}"
                     in "HTTP/\{versionStr}"

export
httpVersionFromString : String -> Maybe (mjr ** mnr ** HTTPVersion mjr mnr)
httpVersionFromString str =
  case str of
       "0.9" => Just (0 ** 9 ** HTTP09)
       "1.0" => Just (1 ** 0 ** HTTP10)
       "1.1" => Just (1 ** 1 ** HTTP11)
       "2" => Just (2 ** 0 ** HTTP20)
       "3" => Just (3 ** 0 ** HTTP30)
       _ => Nothing

export
httpVersionParser : Parser (mjr ** mnr ** HTTPVersion mjr mnr)
httpVersionParser = do
  Just (mjr ** mnr ** version) <- httpVersionFromString <$> takeWhile (/= '\r')
  | Nothing => fail "Invalid HTTP version"
  pure (mjr ** mnr ** version)
