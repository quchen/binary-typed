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
      tests = [ error_coercions_simple
              , error_coercions_complicated
              ]





-- #############################################################################
-- ###  Simple bad type coercion  ##############################################
-- #############################################################################



-- | Decode a typed value as something else.
wackyCoercion :: (Binary a, Typeable a, Binary b, Typeable b)
              => TypeFormat
              -> a
              -> Either String b
wackyCoercion format value = decodeTyped (encodeTyped format value)



-- | Test whether encoding an Int and decoding it as Bool produces a type error
error_coercions_simple :: TestTree
error_coercions_simple = tree tests where
      tree = testGroup "Encode Int, decode Bool => Type error"
      tests = [ error_int_bool_hashed
              , error_int_bool_shown
              , error_int_bool_hashedshown
              , error_int_bool_full
              ]

-- | See 'error_coercions_simple'
error_int_bool_hashed :: TestTree
error_int_bool_hashed =
      testCase "Hashed" $

      isLeft (wackyCoercion Hashed (123 :: Int) :: Either String Bool)
      @?
      "No type error when coercing Int to Bool (with hashed type info)"

-- | See 'error_coercions_simple'
error_int_bool_shown :: TestTree
error_int_bool_shown =
      testCase "Shown" $

      isLeft (wackyCoercion Shown (123 :: Int) :: Either String Bool)
      @?
      "No type error when coercing Int to Bool (with shown type info)"

-- | See 'error_coercions_simple'
error_int_bool_hashedshown :: TestTree
error_int_bool_hashedshown =
      testCase "Hashed+Shown" $

      isLeft (wackyCoercion HashedShown (123 :: Int) :: Either String Bool)
      @?
      "No type error when coercing Int to Bool (with hashed+shown type info)"

-- | See 'error_coercions_simple'
error_int_bool_full :: TestTree
error_int_bool_full =
      testCase "Full" $

      isLeft (wackyCoercion Full (123 :: Int) :: Either String Bool)
      @?
      "No type error when coercing Int to Bool (with full type info)"





-- #############################################################################
-- ###  Complicated bad type coercion  #########################################
-- #############################################################################



-- | Test whether doing a coercion of a complicated type with a small
--   discrepancy produces a type error
error_coercions_complicated :: TestTree
error_coercions_complicated = tree tests where
      tree = testGroup "Complicated type coercion with small discrepancy"
      tests = [ error_long_type_hashed
              , error_long_type_shown
              , error_long_type_hashedshown
              , error_long_type_full
              ]



-- | See 'error_coercions_complicated'
error_long_type_hashed :: TestTree
error_long_type_hashed =
      testCase "Hashed" $

      isLeft (wackyCoercion Hashed long_type_input `asTypeOf` long_type_output)
      @?
      "No type error doing a complicated coercion (with hashed type info)"



-- | See 'error_coercions_complicated'
error_long_type_shown :: TestTree
error_long_type_shown =
      testCase "Shown" $

      isLeft (wackyCoercion Shown long_type_input `asTypeOf` long_type_output)
      @?
      "No type error doing a complicated coercion (with shown type info)"



-- | See 'error_coercions_complicated'
error_long_type_hashedshown :: TestTree
error_long_type_hashedshown =
      testCase "Hashed+Shown" $

      isLeft (wackyCoercion HashedShown long_type_input `asTypeOf` long_type_output)
      @?
      "No type error doing a complicated coercion (with hashed+shown type info)"



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
long_type_output = undefined