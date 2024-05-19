module ByteWeave.HttpVersion

import Data.String.Parser

public export
data HttpVersion : Nat -> Nat -> Type where
  Http09 : HttpVersion 0 9
  Http10 : HttpVersion 1 0
  Http11 : HttpVersion 1 1
  Http20 : HttpVersion 2 0
  Http30 : HttpVersion 3 0

export
{mjr : _} -> {mnr : _} -> Show (HttpVersion mjr mnr) where
  show {mjr} {mnr} version =
    let versionStr = if mjr >= 2
                       then show mjr
                       else "\{show mjr}.\{show mnr}"
                     in "HTTP/\{versionStr}"

export
httpVersionFromString : String -> Maybe (mjr ** mnr ** HttpVersion mjr mnr)
httpVersionFromString str =
  case str of
       "0.9" => Just (0 ** 9 ** Http09)
       "1.0" => Just (1 ** 0 ** Http10)
       "1.1" => Just (1 ** 1 ** Http11)
       "2" => Just (2 ** 0 ** Http20)
       "3" => Just (3 ** 0 ** Http30)
       _ => Nothing

export
httpVersionParser : Parser (mjr ** mnr ** HttpVersion mjr mnr)
httpVersionParser = do
  Just (mjr ** mnr ** version) <- httpVersionFromString <$> takeWhile (/= '\r')
  | Nothing => fail "Invalid HTTP version"
  pure (mjr ** mnr ** version)
