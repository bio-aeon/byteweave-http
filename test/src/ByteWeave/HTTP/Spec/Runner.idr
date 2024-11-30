module ByteWeave.HTTP.Spec.Runner

import System
import Control.ANSI
import Control.App

import ByteWeave.HTTP.Logging
import ByteWeave.HTTP.Spec.ANSI
import ByteWeave.HTTP.Spec.SpecSummary
import ByteWeave.HTTP.Spec.SpecTree
import ByteWeave.HTTP.Spec.SpecResult

evaluateLabel : SpecLabel -> (level : Nat) -> String
evaluateLabel (Describe msg) level = format msg White level
evaluateLabel (It msg)       level = format ("+ " ++ msg) White level

evaluateTree : SpecTree ->
               SpecSummary -> 
               (around : IO SpecResult -> IO SpecResult) ->
               (saveOut : Bool) -> 
               (level : Nat) -> 
               IO SpecSummary

evaluateTree (Leaf (Left info)) state _ saveOut level = let out = evaluateLabel info level in
                                                            if saveOut then
                                                              pure $ addLine out state
                                                            else do 
                                                              run $ logStringStdout <& out
                                                              pure state

evaluateTree (Leaf (Right specIO)) state around saveOut level = resultToSummary !(around specIO) state saveOut level

evaluateTree (Node left right) state around saveOut level 
  = case left of
        (Leaf _) => do newState <- evaluateTree left state around saveOut (level + 1)
                       evaluateTree right newState around saveOut (level + 1)
                       
        _        => do newState <- evaluateTree left state around saveOut level
                       evaluateTree right newState around saveOut level

evaluateResult : (around : IO SpecResult -> IO SpecResult) -> (saveOut : Bool) -> SpecTree -> IO SpecSummary
evaluateResult around saveOut tree = evaluateTree tree neutral around saveOut 0

specWithState : {default False saveOut : Bool} -> SpecTree -> IO SpecSummary
specWithState {saveOut} tree
  = evaluateResult (\spec => spec) saveOut tree

export
spec : SpecTree -> IO ()
spec tree
  = do state <- specWithState tree
       run $ logStringStdout <& "\n" ++ summaryToStr state
       if (failed state) > 0 then exitFailure else pure ()
