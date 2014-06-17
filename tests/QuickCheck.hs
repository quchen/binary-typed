{-# LANGUAGE NumDecimals #-}

module QuickCheck (props) where


import Control.Applicative

import Data.Binary.Typed
import Data.Binary.Typed.Internal

import Test.Tasty
import Test.Tasty.QuickCheck



-- | The entire QuickCheck test tree, to be imported qualified
props :: TestTree
props = tree tests where
      tree = testGroup "QuickCheck"
      tests = [ prop_hashfunction_total
              , prop_inverses
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

      tests = [ testProperty "Untyped"      (prop Untyped)
              , testProperty "Hashed"       (prop Hashed)
              , testProperty "Shown"        (prop Shown)
              , testProperty "Full"         (prop Full)
              ]

      prop :: TypeFormat -> Int -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i



-- | Check whether encoding and decoding a String works properly
prop_inverses_string :: TestTree
prop_inverses_string = tree tests where

      tree = localOption (QuickCheckMaxSize 100)
           . localOption (QuickCheckTests 1e3)
           . testGroup "String"

      tests = [ testProperty "Untyped"      (prop Untyped)
              , testProperty "Hashed"       (prop Hashed)
              , testProperty "Shown"        (prop Shown)
              , testProperty "Full"         (prop Full)
              ]

      prop :: TypeFormat -> String -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i

-- | Generate lots of hashes from random 'typeRep's and see whether one of them
--   crashes.
prop_hashfunction_total :: TestTree
prop_hashfunction_total = tree tests where
      tree = localOption (QuickCheckTests 1e5)
           . localOption (QuickCheckMaxSize 1e3)
           . testGroup "Hash function total?"
      tests = [ testProperty "Random hashes" prop ]

      prop = forAll typeRepGen
                    (\tyCon -> hashType (unStripTypeRep tyCon) `seq` True)



tyConGen :: Gen TyCon
tyConGen = TyCon <$> arbitrary <*> arbitrary <*> arbitrary

typeRepGen :: Gen TypeRep
typeRepGen = TypeRep <$> tyConGen <*> args
      where args = listOf (modifySize (`div` (2::Int)) typeRepGen)
            modifySize f gen = sized (\n -> resize (f n) gen)