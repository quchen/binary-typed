module Main where

import Test.Tasty
import qualified QuickCheck
import qualified HUnit
import qualified APIConsistency as API



main :: IO ()
main = defaultMain (testGroup "binary-typed testsuite"
      [ QuickCheck.props
      , HUnit.props
      , API.props
      ])
