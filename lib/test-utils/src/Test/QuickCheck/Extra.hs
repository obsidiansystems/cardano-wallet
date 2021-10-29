{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Extra helper functions for QuickCheck
--

module Test.QuickCheck.Extra
    (
      -- * Generation
      genFunction
    , genMapWith
    , genSized2
    , genSized2With
    , reasonablySized

      -- * Shrinking
    , liftShrink3
    , liftShrink4
    , liftShrink5
    , liftShrink6
    , liftShrink7
    , liftShrink8
    , liftShrink9
    , shrinkInterleaved
    , shrinkMapWith

      -- * Generating and shrinking natural numbers
    , chooseNatural
    , shrinkNatural

      -- * Counterexamples
    , report
    , verify

      -- * Pretty-printing
    , Pretty (..)

      -- * Combinators
    , NotNull (..)

      -- * Utilities
    , interleaveRoundRobin

    ) where

import Prelude

import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Positions
    ( HasPosition', position' )
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Fmt
    ( indentF, (+|), (|+) )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , Testable
    , chooseInteger
    , counterexample
    , liftArbitrary2
    , liftShrink2
    , listOf
    , property
    , scale
    , shrinkIntegral
    , shrinkList
    , shrinkMapBy
    , suchThat
    , suchThatMap
    , (.&&.)
    )
import Test.QuickCheck.Gen.Unsafe
    ( promote )
import Test.Utils.Pretty
    ( pShowBuilder )
import Text.Pretty.Simple
    ( pShow )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL

-- | Resize a generator to grow with the size parameter, but remains reasonably
-- sized. That is handy when testing on data-structures that can be arbitrarily
-- large and, when large entities don't really bring any value to the test
-- itself.
--
-- It uses a square root function which makes the size parameter grows
-- quadratically slower than normal. That is,
--
--     +-------------+------------------+
--     | Normal Size | Reasonable Size  |
--     | ----------- + ---------------- +
--     | 0           | 0                |
--     | 1           | 1                |
--     | 10          | 3                |
--     | 100         | 10               |
--     | 1000        | 31               |
--     +-------------+------------------+
--
reasonablySized :: Gen a -> Gen a
reasonablySized = scale (ceiling . sqrt @Double . fromIntegral)

-- | Resizes a generator by taking the nth root of the size parameter.
--
-- This combinator can restore size linearity to generators composed of 'n'
-- independent generators in the case that each generator generates values
-- from a range that depends on the size parameter.
--
-- Example:
--
-- Suppose that we have a single generator composed of **three** independent
-- generators, where each generator depends on the size parameter.
--
-- If the current value of the size parameter is 1000, then to generate a range
-- of up to 1000 different composite values, we can resize each individual
-- generator so that it generates up to 10 different values:
--
-- >>> genComposite = Composite
-- >>>     <$> scaleToRoot 3 genA
-- >>>     <*> scaleToRoot 3 genB
-- >>>     <*> scaleToRoot 3 genC
--
scaleToRoot :: Int -> Gen a -> Gen a
scaleToRoot n = scale
    $ floor @Double @Int
    . (** (1.0 / fromIntegral @Int @Double n))
    . fromIntegral @Int @Double

-- | Generates a 2-tuple whose range depends linearly on the size parameter.
--
genSized2 :: Gen a -> Gen b -> Gen (a, b)
genSized2 genA genB = (,)
    <$> scaleToRoot 2 genA
    <*> scaleToRoot 2 genB

-- | Similar to 'genSized2', but with a custom constructor.
--
genSized2With :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
genSized2With f genA genB = uncurry f <$> genSized2 genA genB

-- | Similar to 'liftShrink2', but applicable to 3-tuples.
--
liftShrink3
    :: (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a1, a2, a3)
    -> [(a1, a2, a3)]
liftShrink3 s1 s2 s3 (a1, a2, a3) =
    interleaveRoundRobin
    [ [ (a1', a2 , a3 ) | a1' <- s1 a1 ]
    , [ (a1 , a2', a3 ) | a2' <- s2 a2 ]
    , [ (a1 , a2 , a3') | a3' <- s3 a3 ]
    ]

-- | Similar to 'liftShrink2', but applicable to 4-tuples.
--
liftShrink4
    :: (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a1, a2, a3, a4)
    -> [(a1, a2, a3, a4)]
liftShrink4 s1 s2 s3 s4 (a1, a2, a3, a4) =
    interleaveRoundRobin
    [ [ (a1', a2 , a3 , a4 ) | a1' <- s1 a1 ]
    , [ (a1 , a2', a3 , a4 ) | a2' <- s2 a2 ]
    , [ (a1 , a2 , a3', a4 ) | a3' <- s3 a3 ]
    , [ (a1 , a2 , a3 , a4') | a4' <- s4 a4 ]
    ]

-- | Similar to 'liftShrink2', but applicable to 5-tuples.
--
liftShrink5
    :: (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a1, a2, a3, a4, a5)
    -> [(a1, a2, a3, a4, a5)]
liftShrink5 s1 s2 s3 s4 s5 (a1, a2, a3, a4, a5) =
    interleaveRoundRobin
    [ [ (a1', a2 , a3 , a4 , a5 ) | a1' <- s1 a1 ]
    , [ (a1 , a2', a3 , a4 , a5 ) | a2' <- s2 a2 ]
    , [ (a1 , a2 , a3', a4 , a5 ) | a3' <- s3 a3 ]
    , [ (a1 , a2 , a3 , a4', a5 ) | a4' <- s4 a4 ]
    , [ (a1 , a2 , a3 , a4 , a5') | a5' <- s5 a5 ]
    ]

-- | Similar to 'liftShrink2', but applicable to 6-tuples.
--
liftShrink6
    :: (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a6 -> [a6])
    -> (a1, a2, a3, a4, a5, a6)
    -> [(a1, a2, a3, a4, a5, a6)]
liftShrink6 s1 s2 s3 s4 s5 s6 (a1, a2, a3, a4, a5, a6) =
    interleaveRoundRobin
    [ [ (a1', a2 , a3 , a4 , a5 , a6 ) | a1' <- s1 a1 ]
    , [ (a1 , a2', a3 , a4 , a5 , a6 ) | a2' <- s2 a2 ]
    , [ (a1 , a2 , a3', a4 , a5 , a6 ) | a3' <- s3 a3 ]
    , [ (a1 , a2 , a3 , a4', a5 , a6 ) | a4' <- s4 a4 ]
    , [ (a1 , a2 , a3 , a4 , a5', a6 ) | a5' <- s5 a5 ]
    , [ (a1 , a2 , a3 , a4 , a5 , a6') | a6' <- s6 a6 ]
    ]

-- | Similar to 'liftShrink2', but applicable to 7-tuples.
--
liftShrink7
    :: (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a6 -> [a6])
    -> (a7 -> [a7])
    -> (a1, a2, a3, a4, a5, a6, a7)
    -> [(a1, a2, a3, a4, a5, a6, a7)]
liftShrink7 s1 s2 s3 s4 s5 s6 s7 (a1, a2, a3, a4, a5, a6, a7) =
    interleaveRoundRobin
    [ [ (a1', a2 , a3 , a4 , a5 , a6 , a7 ) | a1' <- s1 a1 ]
    , [ (a1 , a2', a3 , a4 , a5 , a6 , a7 ) | a2' <- s2 a2 ]
    , [ (a1 , a2 , a3', a4 , a5 , a6 , a7 ) | a3' <- s3 a3 ]
    , [ (a1 , a2 , a3 , a4', a5 , a6 , a7 ) | a4' <- s4 a4 ]
    , [ (a1 , a2 , a3 , a4 , a5', a6 , a7 ) | a5' <- s5 a5 ]
    , [ (a1 , a2 , a3 , a4 , a5 , a6', a7 ) | a6' <- s6 a6 ]
    , [ (a1 , a2 , a3 , a4 , a5 , a6 , a7') | a7' <- s7 a7 ]
    ]

-- | Similar to 'liftShrink2', but applicable to 8-tuples.
--
liftShrink8
    :: HasFields8 r a1 a2 a3 a4 a5 a6 a7 a8
    => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r)
    -> (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a6 -> [a6])
    -> (a7 -> [a7])
    -> (a8 -> [a8])
    -> r
    -> [r]
liftShrink8 f s1 s2 s3 s4 s5 s6 s7 s8 r =
    interleaveRoundRobin
    [ [ f a1' a2  a3  a4  a5  a6  a7  a8  | a1' <- s1 a1 ]
    , [ f a1  a2' a3  a4  a5  a6  a7  a8  | a2' <- s2 a2 ]
    , [ f a1  a2  a3' a4  a5  a6  a7  a8  | a3' <- s3 a3 ]
    , [ f a1  a2  a3  a4' a5  a6  a7  a8  | a4' <- s4 a4 ]
    , [ f a1  a2  a3  a4  a5' a6  a7  a8  | a5' <- s5 a5 ]
    , [ f a1  a2  a3  a4  a5  a6' a7  a8  | a6' <- s6 a6 ]
    , [ f a1  a2  a3  a4  a5  a6  a7' a8  | a7' <- s7 a7 ]
    , [ f a1  a2  a3  a4  a5  a6  a7  a8' | a8' <- s8 a8 ]
    ]
  where
    (a1, a2, a3, a4, a5, a6, a7, a8) = toTuple8 r

-- | Similar to 'liftShrink2', but applicable to 9-tuples.
--
liftShrink9
    :: HasFields9 r a1 a2 a3 a4 a5 a6 a7 a8 a9
    => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r)
    -> (a1 -> [a1])
    -> (a2 -> [a2])
    -> (a3 -> [a3])
    -> (a4 -> [a4])
    -> (a5 -> [a5])
    -> (a6 -> [a6])
    -> (a7 -> [a7])
    -> (a8 -> [a8])
    -> (a9 -> [a9])
    -> r
    -> [r]
liftShrink9 f s1 s2 s3 s4 s5 s6 s7 s8 s9 r =
    interleaveRoundRobin
    [ [ f a1' a2  a3  a4  a5  a6  a7  a8  a9  | a1' <- s1 a1 ]
    , [ f a1  a2' a3  a4  a5  a6  a7  a8  a9  | a2' <- s2 a2 ]
    , [ f a1  a2  a3' a4  a5  a6  a7  a8  a9  | a3' <- s3 a3 ]
    , [ f a1  a2  a3  a4' a5  a6  a7  a8  a9  | a4' <- s4 a4 ]
    , [ f a1  a2  a3  a4  a5' a6  a7  a8  a9  | a5' <- s5 a5 ]
    , [ f a1  a2  a3  a4  a5  a6' a7  a8  a9  | a6' <- s6 a6 ]
    , [ f a1  a2  a3  a4  a5  a6  a7' a8  a9  | a7' <- s7 a7 ]
    , [ f a1  a2  a3  a4  a5  a6  a7  a8' a9  | a8' <- s8 a8 ]
    , [ f a1  a2  a3  a4  a5  a6  a7  a8  a9' | a9' <- s9 a9 ]
    ]
  where
    (a1, a2, a3, a4, a5, a6, a7, a8, a9) = toTuple9 r

-- Interleaves the given lists together in round-robin order.
--
-- Examples:
--
-- >>> interleaveRoundRobin [["a1", "a2"], ["b1", "b2"]]
-- ["a1", "b1", "a2", "b2"]
--
-- >>> interleaveRoundRobin [["a1", "a2", "a3"], ["b1", "b2"], ["c1"]]
-- ["a1", "b1", "c1", "a2", "b2", "a3"]
--
interleaveRoundRobin :: [[a]] -> [a]
interleaveRoundRobin = concat . L.transpose

-- | Shrink the given pair in interleaved fashion.
--
-- Successive shrinks of the left and right hand sides are interleaved in the
-- resulting sequence, to avoid biasing either side.
--
shrinkInterleaved :: (a, a -> [a]) -> (b, b -> [b]) -> [(a, b)]
shrinkInterleaved (a, shrinkA) (b, shrinkB) = interleave
    [ (a', b ) | a' <- shrinkA a ]
    [ (a , b') | b' <- shrinkB b ]
  where
    interleave (x : xs) (y : ys) = x : y : interleave xs ys
    interleave xs [] = xs
    interleave [] ys = ys

--------------------------------------------------------------------------------
-- Generating and shrinking natural numbers
--------------------------------------------------------------------------------

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural (lo, hi) =
    chooseInteger (intCast lo, intCast hi)
    `suchThatMap`
    intCastMaybe @Integer @Natural

shrinkNatural :: Natural -> [Natural]
shrinkNatural n
    = mapMaybe (intCastMaybe @Integer @Natural)
    $ shrinkIntegral
    $ intCast n

--------------------------------------------------------------------------------
-- Generating functions
--------------------------------------------------------------------------------

-- | Generates a function.
--
-- This is based on the implementation of 'Arbitrary' for 'a -> b'.
--
genFunction :: (a -> Gen b -> Gen b) -> Gen b -> Gen (a -> b)
genFunction coarbitraryFn gen = promote (`coarbitraryFn` gen)

--------------------------------------------------------------------------------
-- Generating and shrinking key-value maps
--------------------------------------------------------------------------------

-- | Generates a 'Map' with the given key and value generation functions.
--
genMapWith :: Ord k => Gen k -> Gen v -> Gen (Map k v)
genMapWith genKey genValue =
    Map.fromList <$> listOf (liftArbitrary2 genKey genValue)

-- | Shrinks a 'Map' with the given key and value shrinking functions.
--
shrinkMapWith
    :: Ord k
    => (k -> [k])
    -> (v -> [v])
    -> Map k v
    -> [Map k v]
shrinkMapWith shrinkKey shrinkValue
    = shrinkMapBy Map.fromList Map.toList
    $ shrinkList
    $ liftShrink2 shrinkKey shrinkValue

--------------------------------------------------------------------------------
-- Counterexamples
--------------------------------------------------------------------------------

-- | Adds a named variable to the counterexample output of a property.
--
-- On failure, uses pretty-printing to show the contents of the variable.
--
report :: (Show a, Testable prop) => a -> String -> prop -> Property
report a name = counterexample $
    "" +|name|+ ":\n" +|indentF 4 (pShowBuilder a) |+ ""

-- | Adds a named condition to a property.
--
-- On failure, reports the name of the condition that failed.
--
verify :: Bool -> String -> Property -> Property
verify condition conditionTitle =
    (.&&.) (counterexample counterexampleText $ property condition)
  where
    counterexampleText = "Condition violated: " <> conditionTitle

--------------------------------------------------------------------------------
-- Pretty-printing
--------------------------------------------------------------------------------

-- | A combinator that causes the output of `show` to be pretty-printed.
--
newtype Pretty a = Pretty { unPretty :: a }
    deriving Eq

instance Show a => Show (Pretty a) where
    show (Pretty a) = TL.unpack ("\n" <> pShow a <> "\n")

instance Arbitrary a => Arbitrary (Pretty a) where
    arbitrary = Pretty <$> arbitrary
    shrink (Pretty a) = Pretty <$> shrink a

--------------------------------------------------------------------------------
-- Non-null values
--------------------------------------------------------------------------------

newtype NotNull a = NotNull { unNotNull :: a }
    deriving (Eq, Show)

instance (Arbitrary a, Eq a, Monoid a) => Arbitrary (NotNull a) where
    arbitrary = NotNull <$> arbitrary `suchThat` (/= mempty)
    shrink (NotNull u) = NotNull <$> filter (/= mempty) (shrink u)

--------------------------------------------------------------------------------
-- Generic constraints
--------------------------------------------------------------------------------

type HasFields1 r a =
    (HasPosition' 1 r a)

type HasFields2 r a b =
    (HasFields1 r a, HasPosition' 2 r b)

type HasFields3 r a b c =
    (HasFields2 r a b, HasPosition' 3 r c)

type HasFields4 r a b c d =
    (HasFields3 r a b c, HasPosition' 4 r d)

type HasFields5 r a b c d e =
    (HasFields4 r a b c d, HasPosition' 5 r e)

type HasFields6 r a b c d e f =
    (HasFields5 r a b c d e, HasPosition' 6 r f)

type HasFields7 r a b c d e f g =
    (HasFields6 r a b c d e f, HasPosition' 7 r g)

type HasFields8 r a b c d e f g h =
    (HasFields7 r a b c d e f g, HasPosition' 8 r h)

type HasFields9 r a b c d e f g h i =
    (HasFields8 r a b c d e f g h, HasPosition' 9 r i)

toTuple1 :: HasFields1 r a => r -> (a)
toTuple1 r = (r ^. position' @1)

toTuple2 :: HasFields2 r a b => r -> (a, b)
toTuple2 r = (a, r ^. position' @2)
  where
    (a) = toTuple1 r

toTuple3 :: HasFields3 r a b c => r -> (a, b, c)
toTuple3 r = (a, b, r ^. position' @3)
  where
    (a, b) = toTuple2 r

toTuple4 :: HasFields4 r a b c d => r -> (a, b, c, d)
toTuple4 r = (a, b, c, r ^. position' @4)
  where
    (a, b, c) = toTuple3 r

toTuple5 :: HasFields5 r a b c d e => r -> (a, b, c, d, e)
toTuple5 r = (a, b, c, d, r ^. position' @5)
  where
    (a, b, c, d) = toTuple4 r

toTuple6 :: HasFields6 r a b c d e f => r -> (a, b, c, d, e, f)
toTuple6 r = (a, b, c, d, e, r ^. position' @6)
  where
    (a, b, c, d, e) = toTuple5 r

toTuple7 :: HasFields7 r a b c d e f g => r -> (a, b, c, d, e, f, g)
toTuple7 r = (a, b, c, d, e, f, r ^. position' @7)
  where
    (a, b, c, d, e, f) = toTuple6 r

toTuple8 :: HasFields8 r a b c d e f g h => r -> (a, b, c, d, e, f, g, h)
toTuple8 r = (a, b, c, d, e, f, g, r ^. position' @8)
  where
    (a, b, c, d, e, f, g) = toTuple7 r

toTuple9 :: HasFields9 r a b c d e f g h i => r -> (a, b, c, d, e, f, g, h, i)
toTuple9 r = (a, b, c, d, e, f, g, h, r ^. position' @9)
  where
    (a, b, c, d, e, f, g, h) = toTuple8 r
