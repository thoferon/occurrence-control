module Test.OccurrenceControl.Flattening
  ( flatten
  ) where

import Control.Applicative.Free

import Data.List
import Data.Void
import Data.Ratio

import System.Random

import Test.OccurrenceControl.Functor

-- | Flatten the generator by replacing all calls to `listOC` by several calls
-- to the underlying generators.
flatten :: StdGen -> OC (Ratio Int) (Int, Int) g a -> OC (Ratio Int) Void g a
flatten gen = runAp $ \case
  Decision f l r -> liftAp $ Decision f l r
  ListOf (l, u) sub -> do
    let (rn, gen') = next gen
        n = l + rn `mod` (u-l)
        step (_,0) = Nothing
        step (g,m) =
          let (g',g'') = split g
          in Just (flatten g' sub, (g'',m-1))
    sequenceA $ unfoldr step (gen', n)
  LiftGen g -> liftAp $ LiftGen g
