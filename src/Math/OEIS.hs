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

-- | Euler up/down numbers: e.g.f. \(\sec(x) + \tan(x)\), i.e.
-- [A000111](https://oeis.org/A000111).
a000111 :: (Integral a) => Infinite a
a000111 = Infinite.interleave a000364 a000182

-- | Tangent (or \"Zag\") numbers: e.g.f. \(\tan(x)\), also (up to signs) e.g.f.
-- \(\tanh(x)\), i.e. [A000182](https://oeis.org/A000182).
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

-- | See 'a000217'.
triangularNumbers :: (Integral a) => Infinite a
triangularNumbers = Infinite.map a000217 (0 ...)

a000290, squares :: (Enum a, Num a) => Infinite a
a000290 = Infinite.scanl (+) 0 ((1, 3) ....)
-- ^ The squares: \(a000290(n) = n^2\), i.e. [A000290](https://oeis.org/A000290).
squares = a000290
-- ^ See 'a000290'.

-- | Euler (or secant or \"Zig\") numbers: e.g.f. (even powers only)
-- \(\sec(x) = \frac{1}{\cos(x)}\), i.e. [A000364](https://oeis.org/A000364).
a000364 :: (Integral a) => Infinite a
a000364 = everyOther (Infinite.map abs euler)

-- | Oblong (or promic, pronic, or heteromecic) numbers:
-- \(a002378(n) = n(n+1)\), i.e. [A002378](https://oeis.org/A002378).
a002378 :: (Integral a) => a -> a
a002378 n = n * (n + 1)

-- | Length of shortest addition chain for \(n\), i.e.
-- [A003313](https://oeis.org/A003313).
a003313 :: Natural -> Int
a003313 n
  | n `elem` [77, 154] = k - 1
  | otherwise = k
  where
    k = length (powerTree n) - 1

-- | Largest squarefree number diving \(n\): the square free kernel of \(n\),
-- \(\text{rad}(n)\), radical of \(n\), i.e. [A007947](https://oeis.org/A007947).
a007947 :: (UniqueFactorisation a) => a -> a
a007947 = product . distinctPrimeFactors

-- | The [distinct prime factors](https://mathworld.wolfram.com/DistinctPrimeFactors.html)
-- of a given number.
--
-- >>> distinctPrimeFactors 504
-- 2 :| [3,7]
--
-- See also 'a007947' and 'a027748'.
distinctPrimeFactors :: (UniqueFactorisation a) => a -> NonEmpty a
distinctPrimeFactors = NE.map (unPrime . fst) . NE.fromList . factorise

a007953, digitSum :: (Integral a) => a -> a
a007953 n
  | n < 10 = n
  | n < 100 = a076314 n
  | otherwise = first a007953 >>> uncurry (+) $ divMod n 10
-- ^ Digital sum of \(n\), i.e. [A007953](https://oeis.org/A007953).
digitSum = a007953
-- ^ See 'a007953'.

-- | Irregular triangle in which first row is \(1\), \(n\)-th row \((n > 1)\)
-- lists distinct prime factors of \(n\), i.e.
-- [A027748](https://oeis.org/A027748).
--
-- See 'distinctPrimeFactors'.
a027748 :: (Enum a, UniqueFactorisation a) => Infinite a
a027748 = 1 :< Infinite.concatMap distinctPrimeFactors (2 ...)

-- | Numbers that are sums of consecutive squares, i.e.
-- [A034705](https://oeis.org/A034705).
--
-- See also 'a000290' and 'squares'.
a034705 :: Infinite Int
a034705 = Infinite.unfoldr go (0, Infinite.tail (Infinite.inits1 a000290), IS.fromList [0])
  where
    go (x, vs :< vss, seen)
      | minSeen < x = (minSeen, (x, vs :< vss, seen'))
      | otherwise = go (w, vss, IS.union seen (IS.fromList (scanl (+) w ws)))
      where
        (w :| ws) = NE.reverse vs
        (minSeen, seen') = IS.deleteFindMin seen

-- | Smallest number whose sum of digits is \(n\), i.e.
-- [A051885](https://oeis.org/A051885).
--
-- See also 'a007953' and 'digitSum'.
a051885 :: (Integral a) => a -> a
a051885 = flip divMod 9 >>> ((+ 1) *** (10 ^) >>> uncurry (*) >>> subtract 1)

-- | Number of divisors of \(n\) that are smaller than \(\sqrt{n}\), i.e.
-- [A056924](https://oeis.org/A056924).
--
-- \(a056924(n) = N(4n)\) from [Project Euler Problem 174: Hollow Square
-- Laminae II](https://projecteuler.net/problem=174).
a056924 :: (Integral a, UniqueFactorisation a) => a -> a
a056924 = (`div` 2) . tau

-- | \(a060735(1) = 1\), \(a060735(2) = 2\); thereafter, \(a060735(n)\) is the
-- smallest number \(m\) not yet in the sequence such that every prime that
-- divides \(a(n-1)\) also divides \(m\), i.e.
-- [A060735](https://oeis.org/A060735).
--
-- See 'a007947'.
a060735 :: (UniqueFactorisation a) => Infinite a
a060735 = Infinite.iterate ((+) `ap` a007947) 2

-- | \(a076314(n) = \lfloor \frac{n}{10} \rfloor + (n \bmod 10)\), i.e.
-- [A0076314](https://oeis.org/A0076314).
a076314 :: (Integral a) => a -> a
a076314 = uncurry (+) . flip divMod 10

-- | Numbers of the form \(p^{q}q^{p}\), with distinct primes \(p\) and \(q\),
-- i.e. [A082949](https://oeis.org/A082949).
a082949 :: Infinite HybridInteger
a082949 = Infinite.unfoldr go (singleton (HybridInteger (nextPrime 2) (nextPrime 3)))
  where
    go seen
      | p' < q = (hi, insert (HybridInteger p' q) seen')
      | otherwise = (hi, seen')
      where
        seen' = insert (HybridInteger p q') rest
        p' = succ p
        q' = succ q
        (hi@(HybridInteger p q), rest) = deleteFindMin seen

-- | Numbers \(k\) such that \(3k^2 + 3k + 1\) is prime, i.e.
-- [A111251](https://oeis.org/A111251).
a111251 :: Infinite Integer
a111251 =
  Infinite.filter isPrime $
    Infinite.map (\k -> 3 * k * k + 3 * k + 1) (1 ...)

-- | The number of base-10 [bouncy numbers](https://projecteuler.net/problem=113)
-- below \(10^{n}\), i.e. [A204692](https://oeis.org/A204692).
a204692 :: (Integral a) => a -> a
a204692 n
  | n < 3 = 0
  | otherwise = 10 ^ n - (choose (n + 10) 10 + choose (n + 9) 9 - 1 - 10 * n)

-- | Number of integer pairs \((x,y,)\) such that \(0 < x < y \leq n\) and
-- \(xy \leq n\), i.e. [A211264](https://oeis.org/A211264).
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
