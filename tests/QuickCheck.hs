{-# LANGUAGE NumDecimals #-}

module QuickCheck (props) where


import BinaryTyped

import Test.Tasty
import Test.Tasty.QuickCheck



-- | The entire QuickCheck test tree, to be imported qualified
props :: TestTree
props = tree tests where
      tree = localOption (QuickCheckTests 1e3)
           . testGroup "QuickCheck"
      tests = [ prop_inverses ]



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

      tests = [ testProperty "Hashed" (prop Hashed)
              , testProperty "Shown"  (prop Shown)
              , testProperty "Full"   (prop Full)
              ]

      prop :: TypeFormat -> Int -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i



-- | Check whether encoding and decoding a String works properly
prop_inverses_string :: TestTree
prop_inverses_string = tree tests where

      tree = localOption (QuickCheckMaxSize 100)
           . testGroup "String"

      tests = [ testProperty "Hashed" (prop Hashed)
              , testProperty "Shown"  (prop Shown)
              , testProperty "Full"   (prop Full)
              ]

      prop :: TypeFormat -> String -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i