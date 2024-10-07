module Math.OEIS.HybridInteger where

import Math.NumberTheory.Primes (Prime, unPrime)

data HybridInteger
  = HybridInteger (Prime Integer) (Prime Integer)

instance Show HybridInteger where
  show (HybridInteger p q) =
    "(" ++ show (unPrime p) ++ ", " ++ show (unPrime q) ++ ")"

instance Eq HybridInteger where
  x == y = integerValue x == integerValue y

instance Ord HybridInteger where
  x <= y = integerValue x <= integerValue y

integerValue :: HybridInteger -> Integer
integerValue (HybridInteger p q) = p' ^ q' * q' ^ p'
  where
    p' = unPrime p
    q' = unPrime q
