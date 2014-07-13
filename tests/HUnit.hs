{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module HUnit (props) where


import Data.Either

import Data.Binary.Typed
import Data.Binary (Binary(..))
import Data.Typeable (Typeable)

import Test.Tasty
import Test.Tasty.HUnit


-- | Used as a dummy placeholder for fields that never carry values.
data X deriving (Typeable)
instance Binary X where
      get = undefined
      put = undefined




-- | The entire HUnit test tree, to be imported qualified
props :: TestTree
props = tree tests where
      tree = testGroup "HUnit"
      tests = [ error_coercions_simple_large_to_small
              , error_coercions_simple_small_to_large
              , error_coercions_complicated
              ]





-- #############################################################################
-- ###  Simple bad type coercions  #############################################
-- #############################################################################



-- | Decode a typed value as something else.
wackyCoercion :: (Binary a, Typeable a, Binary b, Typeable b)
              => TypeFormat
              -> a
              -> Either String b
wackyCoercion format value = decodeTyped (encodeTyped format value)



-- | Test whether encoding an Int and decoding it as Bool produces a type error.
--   This converts a large field (Int) to one that requires only very little
--   memory (Bool).
error_coercions_simple_large_to_small :: TestTree
error_coercions_simple_large_to_small = tree tests where
      tree = testGroup "Encode Int, decode Bool => Type error"
      tests = [ error_int_bool_hashed32
              , error_int_bool_hashed64
              , error_int_bool_shown
              , error_int_bool_full
              ]

-- | See 'error_coercions_simple_large_to_small'
error_int_bool_hashed32 :: TestTree
error_int_bool_hashed32 =
      testCase "Hashed" $

      isLeft (wackyCoercion Hashed32 (123 :: Int) :: Either String Bool)
      @?
      "No type error when coercing Int to Bool (with 32 bit hashed type info)"

-- | See 'error_coercions_simple_large_to_small'
error_int_bool_hashed64 :: TestTree
error_int_bool_hashed64 =
      testCase "Hashed" $

      isLeft (wackyCoercion Hashed64 (123 :: Int) :: Either String Bool)
      @?
      "No type error when coercing Int to Bool (with 64 bit hashed type info)"

-- | See 'error_coercions_simple_large_to_small'
error_int_bool_shown :: TestTree
error_int_bool_shown =
      testCase "Shown" $

      isLeft (wackyCoercion Shown (123 :: Int) :: Either String Bool)
      @?
      "No type error when coercing Int to Bool (with shown type info)"

-- | See 'error_coercions_simple_large_to_small'
error_int_bool_full :: TestTree
error_int_bool_full =
      testCase "Full" $

      isLeft (wackyCoercion Full (123 :: Int) :: Either String Bool)
      @?
      "No type error when coercing Int to Bool (with full type info)"




-- | Test whether encoding an Int and decoding it as Bool produces a type error.
--   This converts a large field (Int) to one that requires only very little
--   memory (Bool).
error_coercions_simple_small_to_large :: TestTree
error_coercions_simple_small_to_large = tree tests where
      tree = testGroup "Encode Bool, decode Int => Type error"
      tests = [ error_bool_int_hashed32
              , error_bool_int_hashed64
              , error_bool_int_shown
              , error_bool_int_full
              ]

-- | See 'error_coercions_simple_small_to_large'
error_bool_int_hashed32 :: TestTree
error_bool_int_hashed32 =
      testCase "Hashed" $

      isLeft (wackyCoercion Hashed32 True :: Either String Int)
      @?
      "No type error when coercing Bool to Int (with 32 bit hashed type info)"

-- | See 'error_coercions_simple_small_to_large'
error_bool_int_hashed64 :: TestTree
error_bool_int_hashed64 =
      testCase "Hashed" $

      isLeft (wackyCoercion Hashed64 True :: Either String Int)
      @?
      "No type error when coercing Bool to Int (with 64 bit hashed type info)"

-- | See 'error_coercions_simple_small_to_large'
error_bool_int_shown :: TestTree
error_bool_int_shown =
      testCase "Shown" $

      isLeft (wackyCoercion Shown True :: Either String Int)
      @?
      "No type error when coercing Bool to Int (with shown type info)"

-- | See 'error_coercions_simple_small_to_large'
error_bool_int_full :: TestTree
error_bool_int_full =
      testCase "Full" $

      isLeft (wackyCoercion Full True :: Either String Int)
      @?
      "No type error when coercing Bool to Int (with full type info)"





-- #############################################################################
-- ###  Complicated bad type coercion  #########################################
-- #############################################################################



-- | Test whether doing a coercion of a complicated type with a small
--   discrepancy produces a type error
error_coercions_complicated :: TestTree
error_coercions_complicated = tree tests where
      tree = testGroup "Complicated type coercion with small discrepancy"
      tests = [ error_long_type_hashed32
              , error_long_type_hashed64
              , error_long_type_shown
              , error_long_type_full
              ]



-- | See 'error_coercions_complicated'
error_long_type_hashed32 :: TestTree
error_long_type_hashed32 =
      testCase "Hashed" $

      isLeft (wackyCoercion Hashed32 long_type_input `asTypeOf` long_type_output)
      @?
      "No type error doing a complicated coercion (with 32 bit hashed type info)"



-- | See 'error_coercions_complicated'
error_long_type_hashed64 :: TestTree
error_long_type_hashed64 =
      testCase "Hashed" $

      isLeft (wackyCoercion Hashed64 long_type_input `asTypeOf` long_type_output)
      @?
      "No type error doing a complicated coercion (with 64 bit hashed type info)"



-- | See 'error_coercions_complicated'
error_long_type_shown :: TestTree
error_long_type_shown =
      testCase "Shown" $

      isLeft (wackyCoercion Shown long_type_input `asTypeOf` long_type_output)
      @?
      "No type error doing a complicated coercion (with shown type info)"

-- | See 'error_coercions_complicated'
error_long_type_full :: TestTree
error_long_type_full =
      testCase "Full" $

      isLeft (wackyCoercion Full long_type_input `asTypeOf` long_type_output)
      @?
      "No type error doing a complicated coercion (with full type info)"



long_type_input ::                (Either (Either X (), Either X (Either String X)) X, String)
long_type_input = (Left (Right (), Right (Left "hello")), "world")       ------ Different types deep down
                                                                         ------
long_type_output :: Either String (Either (Either X (), Either X (Either Char   X)) X, String)
long_type_output = error "long_type_output should never be evaluated, it is\
                         \ only provided for its type"