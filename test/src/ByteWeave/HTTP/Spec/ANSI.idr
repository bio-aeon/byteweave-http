module ByteWeave.HTTP.Spec.ANSI

import Data.Vect
import Control.ANSI

export
indent : (level : Nat) -> String
indent level = foldr (\el, acc => el ++ acc) "" (replicate level "  ")

export
format : String -> Color -> (level : Nat) -> String
format str color level = show $ colored color $ indent level ++ str
