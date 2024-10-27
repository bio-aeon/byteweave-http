module ByteWeave.HTTP.Spec.SpecSummary

import Data.Vect
import Control.ANSI

import ByteWeave.HTTP.Spec.ANSI

public export
record SpecSummary where
  constructor MkSummary
  totalNum : Nat
  failed   : Nat
  pending  : Nat
  output   : Maybe (List String)
  
Eq SpecSummary where
  (==) (MkSummary lTotal lFailed lPend lOut) (MkSummary rTotal rFailed rPend rOut)
    = (lTotal == rTotal) && (lFailed == rFailed) && (lPend == rPend) && (lOut == rOut)

Show SpecSummary where
  show (MkSummary totalNum failed pending _)
    = "Total = " ++ show totalNum 
        ++ ", Failed = " ++ show failed 
        ++ ", Pending = " ++ show pending
        
Semigroup SpecSummary where
  (<+>) (MkSummary lt lf lp lo) (MkSummary rt rf rp ro) = MkSummary (lt + rt) (lf + rf) (lp + rp) (lo <+> ro)

export
Monoid SpecSummary where
  neutral = MkSummary 0 0 0 Nothing

export
addSpec : SpecSummary -> SpecSummary
addSpec summary = {totalNum $= (+ 1)} summary

export
addFailure : SpecSummary -> SpecSummary
addFailure summary = {totalNum $= (+ 1), failed $= (+ 1)} summary

export
addPending : SpecSummary -> SpecSummary
addPending summary = {totalNum $= (+ 1), pending $= (+ 1)} summary

mergeOutput: Maybe (List String) -> Maybe (List String) -> Maybe (List String)
mergeOutput (Just left) (Just right) = Just $ left <+> right
mergeOutput a@(Just left) Nothing    = a
mergeOutput Nothing b@(Just left)    = b
mergeOutput Nothing Nothing          = Nothing

export
addLine : (line : String) -> SpecSummary -> SpecSummary
addLine line summary = {output $= (\o => mergeOutput o (Just [line]))} summary

export
summaryToStr : SpecSummary -> String
summaryToStr summary
  = show $ colored (if failed summary == 0 then Green else Red) $
      indent 1
         ++ (if failed summary == 0 then "Passed" else "Failed") ++ ": "
         ++ show summary
