module ByteWeave.HTTP.Spec.SpecResult

import Control.ANSI
import Control.App

import ByteWeave.HTTP.Logging
import ByteWeave.HTTP.Spec.ANSI
import ByteWeave.HTTP.Spec.SpecSummary

public export
data SpecResult : Type where     
     Pending : (message : Maybe String) -> SpecResult
     Success : SpecResult
     UnaryFailure : Show a => (val : a) -> (reason : String) -> SpecResult
     BinaryFailure : (Show a, Show b) => (left : a) -> (right : b) -> (reason : String) -> SpecResult

Eq SpecResult where
  (==) (Pending aMsg) (Pending bMsg)                           = aMsg == bMsg
  (==) Success Success                                         = True
  (==) (UnaryFailure _ aReason) (UnaryFailure _ bReason)       = aReason == bReason
  (==) (BinaryFailure _ _ aReason) (BinaryFailure _ _ bReason) = aReason == bReason
  (==) _ _ = False


private
show : SpecResult -> (level : Nat) -> String
show (Pending message) _ = case message of
                             (Just msg) => " [] pending: " ++ msg
                             Nothing    => " [] pending"
show Success _ = ""
show (UnaryFailure actual reason) level           = " [x] " ++ reason ++ 
                                                    "\n     " ++ indent(level) ++ "actual: " ++ show actual
show (BinaryFailure actual expected reason) level = " [x] " ++ reason ++ 
                                                    "\n     " ++ indent(level) ++ "actual:   " ++ show actual ++ 
                                                    "\n     " ++ indent(level) ++ "expected: " ++ show expected


handleOutput : String -> SpecSummary -> (saveOut : Bool) -> IO SpecSummary
handleOutput output summary saveOut = if saveOut then
                                        pure $ addLine output summary
                                      else do
                                        run $ logStringStdout <& output
                                        pure summary

export
resultToSummary : SpecResult -> SpecSummary -> (saveOut : Bool) -> (level : Nat) -> IO SpecSummary
resultToSummary r@(Pending msg) summary saveOut level = let output = format (show r $ level + 1) Yellow (level + 1) in
                                                            pure $ addPending !(handleOutput output summary saveOut)

resultToSummary Success summary _ _ = pure $ addSpec summary
resultToSummary r@(UnaryFailure a reason) summary saveOut level    = let output = format (show r $ level + 1) Red (level + 1) in
                                                                         pure $ addFailure !(handleOutput output summary saveOut)
                                                                
resultToSummary r@(BinaryFailure a b reason) summary saveOut level = let output = format (show r $ level + 1) Red (level + 1) in
                                                                         pure $ addFailure !(handleOutput output summary saveOut)
