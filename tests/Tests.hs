{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BinaryTyped

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
      [ quickCheckProps
      ]




quickCheckProps :: TestTree
quickCheckProps = testGroup "(Tested with QuickCheck)"
      [ qcInt
      ]



qcInt :: TestTree
qcInt = testGroup "Using Int: decode.encode = id"
      [ QC.testProperty "Hashed" (prop Hashed)
      , QC.testProperty "Shown"  (prop Shown)
      , QC.testProperty "Full"   (prop Full)
      ]

      where

      prop :: TypeFormat -> Int -> Bool
      prop format i = unsafeDecodeTyped (encodeTyped format i) == i
