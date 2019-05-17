-- | This module provides ways of controlling the probability distribution of
-- the number of occurrences when generating random structures.

{-# LANGUAGE RankNTypes #-}

module Test.OccurrenceControl.Distribution
  ( Distribution
  ) where

import Control.Applicative.Free

import Test.OccurrenceControl.Functor

type Distribution
  = forall g a. OC Int (Int, Int) g a -> OC Bool Int g a

poisson :: Double -> Distribution
poisson k = undefined
