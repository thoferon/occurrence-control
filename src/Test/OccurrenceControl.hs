{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.OccurrenceControl
  ( OccurrenceControl
  , runOccurrenceControl
  , decide
  , decide'
  , decideFactor
  , decideFactor'
  , listOC
  , liftGen
  ) where

import Control.Applicative.Free
import Control.Monad (replicateM)

import Data.Ratio

import Test.OccurrenceControl.Distribution
import Test.OccurrenceControl.Factor
import Test.OccurrenceControl.Functor

runOccurrenceControl
  :: Applicative g => Distribution -> OccurrenceControl g a -> g a
runOccurrenceControl distrib =
    runAp interpret . distrib . factorsToWeights
  where
    interpret :: Applicative g => OCF Bool Int g a -> g a
    interpret = \case
      Decision b l r -> if b then l else r
      ListOf n oc -> replicateM n $ runAp interpret oc
      LiftGen g -> g

-- | Decide whether the event should occur and call the first program if it
-- should, the second one otherwise.
decide :: g a -> g a -> OccurrenceControl g a
decide = decideFactor 1

decide' :: (Bool -> g a) -> OccurrenceControl g a
decide' = decideFactor' 1

-- | Decide whether the event should occur and call the first program if it
-- should, the second one otherwise.
--
-- It takes a factor so that some errors can be made more likely than others.
decideFactor :: Ratio Int -> g a -> g a -> OccurrenceControl g a
decideFactor f l r = liftAp $ Decision f l r

decideFactor' :: Ratio Int -> (Bool -> g a) -> OccurrenceControl g a
decideFactor' f g = decideFactor f (g True) (g False)

-- | Generate a list of a random number of elements within the given range
-- such that the probability of a certain kind of errors happening in any of
-- them stays the same regardless of the number of elements.
listOC
  :: Int -- ^ Lower limit.
  -> Int -- ^ Upper limit.
  -> OccurrenceControl g a -> OccurrenceControl g [a]
listOC l u f = liftAp $ ListOf (l, u) f

-- | Lift a generator into 'OccurrenceControl'.
liftGen :: g a -> OccurrenceControl g a
liftGen = liftAp . LiftGen
