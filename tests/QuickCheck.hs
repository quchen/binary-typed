{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RankNTypes #-}

module QuickCheck (props) where


import Control.Applicative
import Data.Typeable (Typeable)
import Data.Int
import Data.Bits ((.&.), (.|.))

import qualified Data.ByteString.Lazy as BSL
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
      tests = [ prop_inverses
              , prop_api
              , prop_internal
              , prop_sizes
              ]





-- #############################################################################
-- ###  Decode is left-inverse to encode  ######################################
-- #############################################################################



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
           . testGroup "Int"

      tests = [ testProperty "Untyped"  (prop Untyped)
              , testProperty "Hashed5"  (prop Hashed5)
              , testProperty "Hashed32" (prop Hashed32)
              , testProperty "Hashed64" (prop Hashed64)
              , testProperty "Shown"    (prop Shown)
              , testProperty "Full"     (prop Full)
              ]

      prop :: TypeFormat -> Int -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i



-- | Check whether encoding and decoding a String works properly
prop_inverses_string :: TestTree
prop_inverses_string = tree tests where

      tree = localOption (QuickCheckMaxSize 100)
           . testGroup "String"

      tests = [ testProperty "Untyped"  (prop Untyped)
              , testProperty "Hashed5"  (prop Hashed5)
              , testProperty "Hashed32" (prop Hashed32)
              , testProperty "Hashed64" (prop Hashed64)
              , testProperty "Shown"    (prop Shown)
              , testProperty "Full"     (prop Full)
              ]

      prop :: TypeFormat -> String -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i





-- #############################################################################
-- ###  API tests  #############################################################
-- #############################################################################



-- | Check whether the laws mentioned in the docs hold
prop_api :: TestTree
prop_api = tree tests where

      tree = testGroup "API"

      tests = [ testProperty "erase inverse of typed"                 prop_erase
              , testProperty "mapTyped id ~ id"                       prop_mapTyped_id
              , testProperty "mapTyped f.g ~ mapTyped f . mapTyped g" prop_mapTyped_compose
              , testProperty "reType equivalent to reconstruction"    prop_reType
              , testProperty "encodeTyped = encode.typed"             prop_encodeTyped
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
            (unsafeDecodeTyped (encodeTyped format value) :: Int)
            ==
            (unsafeDecodeTyped (encode (typed format value)) :: Int)



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
                  cached  = fmap preserializeTyped plain
                  cached2 = fmap preserializeTyped cached
                  preserializeTyped (Typed ty x) = Typed (preserialize ty) x

instance Arbitrary TypeFormat where
      arbitrary = elements [Untyped, Hashed5, Hashed32, Hashed64, Shown, Full]




-- #############################################################################
-- ###  Internal functions  ####################################################
-- #############################################################################



prop_internal :: TestTree
prop_internal = tree tests where

      tree = testGroup "Internal"

      tests = [ localOption (QuickCheckMaxSize 10)
                            (testProperty "stripTypeRep . unStripTypeRep = id"
                                          prop_stripTypeRep_inverses)
              , testProperty "stripTyCon . unStripTyCon = id"
                             prop_stripTyCon_inverses
              , testProperty "getFormat extracts format correctly"
                             prop_getFormat
              , localOption (QuickCheckMaxSize 10)
                            (testProperty "Hash5 has 3 rightmost bits zero"
                                          prop_hash5_zero)
              , testProperty "hash5Split invertible"
                             prop_hash5Split_invertible
              ]


-- getFormat extracts the right format
prop_getFormat :: Typed Double -> Bool
prop_getFormat t@(Typed ty x) = t `isEqual` typed (getFormat ty) x


prop_stripTypeRep_inverses :: TypeRep -> Bool
prop_stripTypeRep_inverses x = (stripTypeRep . unStripTypeRep) x == x

prop_stripTyCon_inverses :: TyCon -> Bool
prop_stripTyCon_inverses x = (stripTyCon . unStripTyCon) x == x

-- | Does every hashed 'TypeRep' result in a 'Hash5' that has its three
-- rightmost bits zero?
prop_hash5_zero :: TypeRep -> Bool
prop_hash5_zero x = let Hash5 x' = hashType5 (unStripTypeRep x)
                    in  7 Data.Bits..&. x' == 0
                        -- 7 = 00000111

-- | Splitting and putting the numbers together again should be the identity
prop_hash5Split_invertible :: Word8 -> Bool
prop_hash5Split_invertible x =
      let (tag, Hash5 hash) = hashed5Split x
      in  x == tag .|. hash






instance Arbitrary TyCon where
      arbitrary = TyCon <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TypeRep where
      arbitrary = TypeRep <$> arbitrary <*> args
            where args = listOf (modifySize (`div` 2) arbitrary)



-- | Modify the size parameter of a 'Gen'.
modifySize :: (Int -> Int) -> Gen a -> Gen a
modifySize f gen = sized (\n -> resize (f n) gen)





-- #############################################################################
-- ###  Encoding sizes  ########################################################
-- #############################################################################



-- | Are the additional message sizes stated by the docs accurate?
--
-- Untyped:  +1 byte
-- Hashed32: +5 byte
-- Hashed64: +9 byte
prop_sizes :: TestTree
prop_sizes = tree tests where

      tree = testGroup "Data sizes"

      tests = [ testProperty "Untyped:  +1 byte"
                             (prop_size_added (encodeTyped Untyped)  1)
              , testProperty "Hashed5:  +1 byte"
                             (prop_size_added (encodeTyped Hashed5)  1)
              , testProperty "Hashed32: +5 byte"
                             (prop_size_added (encodeTyped Hashed32) 5)
              , testProperty "Hashed64: +9 byte"
                             (prop_size_added (encodeTyped Hashed64) 9)
              ]


type Complicated = Either (Char, Int) (Either String (Maybe Integer))

-- | Check whether data created with a certain format has a certain
--   overhead over the direct Binary serialization.
prop_size_added ::
         (forall a. (Typeable a, Binary a) => a -> BSL.ByteString)
      -> Int64
      -> Property
prop_size_added serializer n =
      conjoin [ forAll arbitrary (verify :: Integer     -> Bool)
              , forAll arbitrary (verify :: Double      -> Bool)
              , forAll arbitrary (verify :: [Double]    -> Bool)
              , forAll arbitrary (verify :: Complicated -> Bool)
              ]

      where

      binSize :: Binary a => a -> Int64
      binSize   = BSL.length . encode

      typedSize :: (Binary a, Typeable a) => a -> Int64
      typedSize = BSL.length . serializer

      verify :: (Binary a, Typeable a) => a -> Bool
      verify x = binSize x + n == typedSize x
