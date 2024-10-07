-- |
-- Module      : Math.OEIS
-- Copyright   : (c) Eric Bailey, 2020-2024
--
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
--
-- Some sequences from [OEIS](https://oeis.org)
module Math.OEIS
  ( a000078,
    a000111,
    a000182,
    a000217,
    a000290,
    a000364,
    a002378,
    a003313,
    a007947,
    a007953,
    a027748,
    a034705,
    a051885,
    a056924,
    a060735,
    a076314,
    a082949,
    a111251,
    a204692,
    a211264,
    digitSum,
    distinctPrimeFactors,
    squares,
    triangularNumbers,
  )
where

import Control.Arrow (first, (***), (>>>))
import Control.Monad (ap, join)
import Data.Foldable (toList)
import qualified Data.IntSet as IS
import Data.List.Infinite (Infinite ((:<)), (...), (....))
import qualified Data.List.Infinite as Infinite
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio (numerator)
import Data.Set (deleteFindMin, insert, singleton)
import Math.Combinatorics.Exact.Binomial (choose)
import Math.NumberTheory.ArithmeticFunctions (tau)
import Math.NumberTheory.Primes (UniqueFactorisation, factorise, nextPrime, unPrime)
import Math.NumberTheory.Primes.Testing (isPrime)
import Math.NumberTheory.Recurrences (bernoulli, euler)
import Math.OEIS.HybridInteger (HybridInteger (..))
import Numeric.Natural (Natural)

-- | Tetranacci numbers, i.e. [A000078](https://oeis.org/A000078).
a000078 :: (Integral a) => Infinite a
a000078 = Infinite.unfoldr go (0, 0, 0, 1)
  where
    go (a, b, c, d) = (a, (b, c, d, a + b + c + d))

-- https://oeis.org/A000111
a000111 :: (Integral a) => Infinite a
a000111 = Infinite.interleave a000364 a000182

-- https://oeis.org/A000182
a000182 :: (Integral a) => Infinite a
a000182 =
  Infinite.map
    (\(n, b) -> numerator (2 ^ n * (2 ^ n - 1) * abs b) `div` n)
    $ Infinite.zip ((2, 4) ....)
    $ Infinite.tail
    $ everyOther bernoulli

-- | Triangular numbers, i.e. [A000217](https://oeis.org/A000217).
a000217 :: (Integral a) => a -> a
a000217 n = n * (n + 1) `div` 2

triangularNumbers :: (Integral a) => Infinite a
triangularNumbers = Infinite.map a000217 (0 ...)

-- https://oeis.org/A000290
a000290, squares :: (Enum a, Num a) => Infinite a
a000290 = Infinite.scanl (+) 0 ((1, 3) ....)
squares = a000290

-- https://oeis.org/A000364
a000364 :: (Integral a) => Infinite a
a000364 = everyOther (Infinite.map abs euler)

-- https://oeis.org/A002378
a002378 :: (Integral a) => a -> a
a002378 n = n * (n + 1)

-- https://oeis.org/A003313
a003313 :: Natural -> Int
a003313 n
  | n `elem` [77, 154] = k - 1
  | otherwise = k
  where
    k = length (powerTree n) - 1

-- https://oeis.org/A007947
a007947 :: (UniqueFactorisation a) => a -> a
a007947 = product . distinctPrimeFactors

-- | The distinct prime factors of a given number.
--
-- >>> distinctPrimeFactors 504
-- 2 :| [3,7]
distinctPrimeFactors :: (UniqueFactorisation a) => a -> NonEmpty a
distinctPrimeFactors = NE.map (unPrime . fst) . NE.fromList . factorise

-- https://oeis.org/A007953
a007953, digitSum :: (Integral a) => a -> a
a007953 n
  | n < 10 = n
  | n < 100 = a076314 n
  | otherwise = first a007953 >>> uncurry (+) $ divMod n 10
digitSum = a007953

-- https://oeis.org/A027748
a027748 :: (Enum a, UniqueFactorisation a) => Infinite a
a027748 = 1 :< Infinite.concatMap distinctPrimeFactors (2 ...)

-- https://oeis.org/A034705
a034705 :: Infinite Int
a034705 = go 0 (Infinite.tail (Infinite.inits1 a000290)) (IS.fromList [0])
  where
    go x (vs :< vss) seen
      | minSeen < x = minSeen :< go x (vs :< vss) seen'
      | otherwise = go w vss (IS.union seen (IS.fromList (scanl (+) w ws)))
      where
        (w :| ws) = NE.reverse vs
        (minSeen, seen') = IS.deleteFindMin seen

-- https://oeis.org/A051885
a051885 :: (Integral a) => a -> a
a051885 = flip divMod 9 >>> ((+ 1) *** (10 ^) >>> uncurry (*) >>> subtract 1)

-- | \(a056924(n) = N(4n)\) from [Project Euler Problem 174: Hollow Square
-- Laminae II](https://projecteuler.net/problem=174), i.e.
-- [A056924](https://oeis.org/A056924).
a056924 :: (Integral a, UniqueFactorisation a) => a -> a
a056924 = (`div` 2) . tau

-- https://oeis.org/A060735
a060735 :: (UniqueFactorisation a) => Infinite a
a060735 = Infinite.iterate ((+) `ap` a007947) 2

-- https://oeis.org/A0076314
a076314 :: (Integral a) => a -> a
a076314 = uncurry (+) . flip divMod 10

-- | Numbers of the form \(p^{q}q^{p}\), with distinct primes \(p\) and \(q\),
-- i.e. [A082949](https://oeis.org/A082949).
a082949 :: Infinite HybridInteger
a082949 = go $ singleton (HybridInteger (nextPrime 2) (nextPrime 3))
  where
    go s = hi :< go (if p' < q then insert (HybridInteger p' q) s'' else s'')
      where
        s'' = insert (HybridInteger p q') s'
        p' = succ p
        q' = succ q
        (hi@(HybridInteger p q), s') = deleteFindMin s

-- | Numbers \(k\) such that \(3k^2 + 3k + 1\) is prime, i.e.
-- [A111251](https://oeis.org/A111251).
a111251 :: Infinite Integer
a111251 =
  Infinite.filter isPrime $
    Infinite.map (\k -> 3 * k * k + 3 * k + 1) (1 ...)

-- https://oeis.org/A204692
a204692 :: (Integral a) => a -> a
a204692 n
  | n < 3 = 0
  | otherwise = 10 ^ n - (choose (n + 10) 10 + choose (n + 9) 9 - 1 - 10 * n)

-- https://oeis.org/A211264
a211264 :: (Integral a) => a -> a
a211264 n = sum [n `div` k | k <- [1 .. m]] - m * (m + 1) `div` 2
  where
    m = floor @Double (sqrt (fromIntegral n))

-- -------------------------------------------------------- [ Helper functions ]

everyOther :: Infinite a -> Infinite a
everyOther (x :< _ :< xs) = x :< everyOther xs

powerTree :: Natural -> [Natural]
powerTree n
  | n <= 0 = []
  | otherwise = findIn levels
  where
    findIn (m :| ms) = maybe (findIn (NE.fromList ms)) toList (Map.lookup n m)

levels :: NonEmpty (Map Natural [Natural])
levels = NE.iterate expand (Map.singleton 1 [1])
  where
    expand = join (Map.foldrWithKey addLevel)
    addLevel n xs tree = foldr (addNext n xs) tree xs
    addNext n xs m = Map.insert (n + m) (n + m : xs)
