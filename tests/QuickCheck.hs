{-# LANGUAGE NumDecimals #-}

module QuickCheck (props) where


import Control.Applicative
import Data.Typeable (Typeable)

import Data.Binary
import Data.Binary.Typed
import Data.Binary.Typed.Internal

import Test.Tasty
import Test.Tasty.QuickCheck



-- | The entire QuickCheck test tree, to be imported qualified
props :: TestTree
props = tree tests where
      tree = testGroup "QuickCheck"
      tests = [ prop_typerep
              , prop_inverses
              , prop_api
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

      tests = [ testProperty "Untyped" (prop Untyped)
              , testProperty "Hashed"  (prop Hashed)
              , testProperty "Shown"   (prop Shown)
              , testProperty "Full"    (prop Full)
              ]

      prop :: TypeFormat -> Int -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i



-- | Check whether encoding and decoding a String works properly
prop_inverses_string :: TestTree
prop_inverses_string = tree tests where

      tree = localOption (QuickCheckMaxSize 100)
           . localOption (QuickCheckTests 1e3)
           . testGroup "String"

      tests = [ testProperty "Untyped" (prop Untyped)
              , testProperty "Hashed"  (prop Hashed)
              , testProperty "Shown"   (prop Shown)
              , testProperty "Full"    (prop Full)
              ]

      prop :: TypeFormat -> String -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i



-- | Test properties of 'TypeRep's and 'TyCon's.
prop_typerep :: TestTree
prop_typerep = tree tests where
      tree = localOption (QuickCheckTests 1e4)
           . localOption (QuickCheckMaxSize 1e3)
           . testGroup "TypeRep, TyCon"

      tests = [ prop_hash_total
              ]



-- | Generate lots of hashes from random 'typeRep's and see whether one of them
--   crashes.
prop_hash_total :: TestTree
prop_hash_total = testProperty "Hash function total" prop where
      prop = forAll arbitrary
                    (\tyCon -> hashType (unStripTypeRep tyCon) `seq` True)



instance Arbitrary TyCon where
      arbitrary = TyCon <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TypeRep where
      arbitrary = TypeRep <$> arbitrary <*> args
            where args = listOf (modifySize (`div` (2::Int)) arbitrary)



-- | Modify the size parameter of a 'Gen'.
modifySize :: (Int -> Int) -> Gen a -> Gen a
modifySize f gen = sized (\n -> resize (f n) gen)



-- | Check whether the laws mentioned in the docs hold
prop_api :: TestTree
prop_api = tree tests where

      tree = testGroup "API"

      tests = [ testProperty "erase"           prop_erase
              , testProperty "reType"          prop_reType
              , testProperty "encodeTyped"     prop_encodeTyped
              , testProperty "encodeTypedLike" prop_encodeTypedLike
              ]

      prop_erase :: TypeFormat -> Int -> Bool
      prop_erase format x = erase (typed format x) == x

      prop_reType :: TypeFormat -> Typed Int -> Bool
      prop_reType format x =
            let (Typed tyA a) = reType format x
                (Typed tyB b) = typed format (erase x)
            in  (tyA, a) == (tyB, b)

      prop_encodeTyped :: TypeFormat -> Int -> Bool
      prop_encodeTyped format value =
            encodeTyped format value == encode (typed format value)

      prop_encodeTypedLike :: Typed Int -> Int -> Bool
      prop_encodeTypedLike ty value =
            (unsafeDecodeTyped (encodeTypedLike ty value) :: Int)
            ==
            unsafeDecodeTyped (encode (reValue (const value) ty))



instance (Arbitrary a, Typeable a) => Arbitrary (Typed a) where
      arbitrary = typed <$> arbitrary <*> arbitrary

instance Arbitrary TypeFormat where
      arbitrary = elements [Untyped, Hashed, Shown, Full]