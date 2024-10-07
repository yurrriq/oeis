-- |
-- Module      : Math.OEIS.HybridInteger
-- Copyright   : (c) Eric Bailey, 2020-2024
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
--
-- See 'Math.OEIS.a082949'.
module Math.OEIS.HybridInteger where

import Data.Function (on)
import Math.NumberTheory.Primes (Prime, unPrime)

-- | A number of the form \(p^{q}q^{p}\), with distinct primes \(p\) and \(q\).
data HybridInteger
  = HybridInteger (Prime Integer) (Prime Integer)

instance Show HybridInteger where
  show (HybridInteger p q) =
    "(" ++ show (unPrime p) ++ ", " ++ show (unPrime q) ++ ")"

instance Eq HybridInteger where
  (==) = (==) `on` hybridIntegerValue

instance Ord HybridInteger where
  (<=) = (<=) `on` hybridIntegerValue

-- | The integer value of a 'HybridInteger', i.e. \(p^{q}q^{p}\).
hybridIntegerValue :: HybridInteger -> Integer
hybridIntegerValue (HybridInteger p q) = p' ^ q' * q' ^ p'
  where
    p' = unPrime p
    q' = unPrime q
