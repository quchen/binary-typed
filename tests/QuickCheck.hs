{-# LANGUAGE NumDecimals #-}

module QuickCheck (props) where


import Control.Applicative
import Data.Typeable (Typeable)
import Data.Int

import Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Binary.Typed
import Data.Binary.Typed.Internal

import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Show.Functions () -- This fixes the missing Show (a->b) instance in
                              -- Travis. Can probably be removed in the future.



-- | The entire QuickCheck test tree, to be imported qualified
props :: TestTree
props = tree tests where
      tree = testGroup "QuickCheck"
      tests = [ prop_typerep
              , prop_inverses
              , prop_api
              , prop_internal
              , prop_sizes
              ]



-- | Check whether typed encoding and decoding are inverses of each other
prop_inverses :: TestTree
prop_inverses = tree tests where
      tree = testGroup "decode.encode = id"
      tests = [ prop_inverses_int
              , prop_inverses_string
              ]



-- | Check whether encoding and decoding an Int works properly
prop_inverses_int :: TestTree
prop_inverses_int = tree tests where

      tree = localOption (QuickCheckMaxSize maxBound)
           . localOption (QuickCheckTests 1e3)
           . testGroup "Int"

      tests = [ testProperty "Untyped"  (prop Untyped)
              , testProperty "Hashed32" (prop Hashed32)
              , testProperty "Hashed64" (prop Hashed64)
              , testProperty "Shown"    (prop Shown)
              , testProperty "Full"     (prop Full)
              , testProperty "Cached"    prop_cached
              ]

      prop :: TypeFormat -> Int -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i

      prop_cached :: Typed Int -> Int -> Bool
      prop_cached dummy i = unsafeDecodeTyped (encodeTypedLike dummy i) == i



-- | Check whether encoding and decoding a String works properly
prop_inverses_string :: TestTree
prop_inverses_string = tree tests where

      tree = localOption (QuickCheckMaxSize 100)
           . localOption (QuickCheckTests 1e3)
           . testGroup "String"

      tests = [ testProperty "Untyped"  (prop Untyped)
              , testProperty "Hashed32" (prop Hashed32)
              , testProperty "Hashed64" (prop Hashed64)
              , testProperty "Shown"    (prop Shown)
              , testProperty "Full"     (prop Full)
              , testProperty "Cached"    prop_cached
              ]

      prop :: TypeFormat -> String -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i

      prop_cached :: Typed String -> String -> Bool
      prop_cached dummy i = unsafeDecodeTyped (encodeTypedLike dummy i) == i



-- | Test properties of 'TypeRep's and 'TyCon's.
prop_typerep :: TestTree
prop_typerep = tree tests where
      tree = localOption (QuickCheckTests 1e3)
           . localOption (QuickCheckMaxSize 10)
           . testGroup "TypeRep, TyCon"

      tests = []



instance Arbitrary TyCon where
      arbitrary = TyCon <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TypeRep where
      arbitrary = TypeRep <$> arbitrary <*> args
            where args = listOf (modifySize (`div` 2) arbitrary)



-- | Modify the size parameter of a 'Gen'.
modifySize :: (Int -> Int) -> Gen a -> Gen a
modifySize f gen = sized (\n -> resize (f n) gen)



-- | Check whether the laws mentioned in the docs hold
prop_api :: TestTree
prop_api = tree tests where

      tree = testGroup "API"

      tests = [ testProperty "erase"            prop_erase
              , testProperty "mapTyped id law"  prop_mapTyped_id
              , testProperty "mapTyped f.g law" prop_mapTyped_compose
              , testProperty "reType"           prop_reType
              , testProperty "encodeTyped"      prop_encodeTyped
              , testProperty "encodeTypedLike"  prop_encodeTypedLike
              ]

      prop_erase :: TypeFormat -> Int -> Bool
      prop_erase format x = erase (typed format x) == x

      prop_mapTyped_id :: Typed Double -> Bool
      prop_mapTyped_id x = x `isEqual` mapTyped id x

      prop_mapTyped_compose
            :: (Int -> Maybe Integer)
            -> (Double -> Int)
            -> Typed Double
            -> Bool
      prop_mapTyped_compose f g x =
            mapTyped (f . g) x `isIdentical` (mapTyped f . mapTyped g) x

      prop_reType :: TypeFormat -> Typed Int -> Bool
      prop_reType format x =reType format x `isIdentical` typed format (erase x)

      prop_encodeTyped :: TypeFormat -> Int -> Bool
      prop_encodeTyped format value =
            encodeTyped format value == encode (typed format value)

      prop_encodeTypedLike :: Typed Int -> Int -> Bool
      prop_encodeTypedLike ty value =
            (unsafeDecodeTyped (encodeTypedLike ty value) :: Int)
            ==
            unsafeDecodeTyped (encode (reValue (const value) ty))



-- | Equality of 'Typed' values, taking only the contained value into account.
--   See also 'isIdentical'.
isEqual :: Eq a => Typed a -> Typed a -> Bool
isEqual (Typed _tyA a) (Typed _tyB b) = a == b



-- | Equality of 'Typed' values, taking the contained type representation into
--   account. This means that a cached and an uncached (otherwise identical)
--   type representation are unequal.
--   See also 'isEqual'.
isIdentical :: Eq a => Typed a -> Typed a -> Bool
isIdentical (Typed tyA a) (Typed tyB b) = (tyA, a) == (tyB, b)



instance (Arbitrary a, Typeable a) => Arbitrary (Typed a) where
      arbitrary = frequency [(10, plain), (5, cached), (3, cached2)]
            where plain = typed <$> arbitrary <*> arbitrary
                  cached  = fmap precache plain
                  cached2 = fmap precache cached

instance Arbitrary TypeFormat where
      arbitrary = elements [Untyped, Hashed32, Hashed64, Shown, Full]



prop_internal :: TestTree
prop_internal = tree tests where

      tree = testGroup "Internal"

      tests = [ testProperty "getFormat" prop_getFormat
              ]

      -- getFormat extracts the right format
      prop_getFormat :: Typed Double -> Bool
      prop_getFormat t@(Typed ty x) = t `isEqual` typed (getFormat ty) x



-- | Are the additional message sizes stated by the docs accurate?
--
-- Untyped: +1 byte
-- Hashed32: +5 byte
-- Hashed64: +9 byte
prop_sizes :: TestTree
prop_sizes = tree tests where

      tree = testGroup "Data sizes"

      tests = [ testProperty "Untyped:  +1 byte" (prop_size_added Untyped 1)
              , testProperty "Hashed32: +5 byte" (prop_size_added Hashed32 5)
              , testProperty "Hashed64: +9 byte" (prop_size_added Hashed64 9)
              ]

      -- | Check whether data created with a certain format has a certain
      --   overhead over the direct Binary serialization.
      prop_size_added :: TypeFormat -> Int64 -> [Double] -> Bool
      prop_size_added format n x = binSize + n == typedSize
            where binSize   = BSL.length (encode x)
                  typedSize = BSL.length (encodeTyped format x)