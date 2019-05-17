module Test.OccurrenceControl.Factor
  ( factorsToWeights
  ) where

import Control.Applicative.Free

import Data.Ratio
import Data.List

import Test.OccurrenceControl.Functor

-- | Transform an 'OC' defines in terms of factors in one defined in terms of
-- weights.
--
-- It is easier to define a 'OC' with factors but it is easier to define a
-- 'Distribution' with weights. This function is the bridge between both.
--
-- 1) Collect all unique denominators.
-- 2) Set all factors with the same denominator.
-- 3) Replace factors by their numerator.
factorsToWeights :: OC (Ratio Int) l g a -> OC Int l g a
factorsToWeights oc =
  let denoms = getUniqueDenominators oc
      toWeight factor =
        let denoms' = delete (denominator factor) denoms
        in foldl' (*) (numerator factor) denoms'

      weightOC :: OC (Ratio Int) l g a -> OC Int l g a
      weightOC = hoistAp $ \case
        Decision f l r -> Decision (toWeight f) l r
        ListOf l sub -> ListOf l (weightOC sub)
        LiftGen g -> LiftGen g

  in weightOC oc

getUniqueDenominators :: OC (Ratio Int) l g a -> [Int]
getUniqueDenominators = nub . go
  where
    go :: OC (Ratio Int) l g a -> [Int]
    go = runAp_ $ \case
      Decision f _ _ -> [denominator f]
      ListOf _ sub -> getUniqueDenominators sub
      LiftGen _ -> []
