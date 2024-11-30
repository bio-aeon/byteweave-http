module ByteWeave.HTTP.Logging

import Control.App
import Control.Monad.Reader.Reader
import Control.Monad.Reader.Interface

data LogAction : (Type -> Type) -> Type -> Type where
  MkLogAction : (log : msg -> m ()) -> LogAction m msg

data LoggerT : Type -> (Type -> Type) -> Type -> Type where
  MkLoggerT : ReaderT a m (LogAction (LoggerT msg m) msg) -> LoggerT a m msg

export
logStringStdout : PrimIO es => LogAction (App es) String
logStringStdout = MkLogAction (primIO . putStrLn)

export infixr 5 <&

export
(<&) : LogAction m msg -> msg -> m ()
(<&) (MkLogAction log) msg = log msg
