{-# LANGUAGE GADTs #-}

module Test.OccurrenceControl.Functor
  ( OCF(..)
  , OC
  , OccurrenceControl
  ) where

import Control.Applicative.Free

import Data.Ratio

data OCF f l g a where
  Decision :: f -> g a -> g a -> OCF f l g a
  ListOf   :: l -> OC f l g a -> OCF f l g [a]
  LiftGen  :: g a -> OCF f l g a

type OC f l g = Ap (OCF f l g)

-- | An applicative functor in which decision about the occurrence of a kind of
-- event can be made. You can get a generator of type @g a@ from an expression
-- of type @OccurrenceControl g a@.
type OccurrenceControl g = OC (Ratio Int) (Int, Int) g
