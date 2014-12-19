-- | This dummy module simply makes sure that "Data.Binary.Typed" and
-- "Data.Binary.Typed.Debug" share the same API, so they can be used
-- interchangably.
--
-- This module will not check anything per se, but will halt the compilation
-- of the testsuite if there's an error.


{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module APIConsistency (props) where


import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Data.Binary.Typed as N
import Data.Binary.Typed.Debug as D
import Data.Binary.Typed.Internal

import qualified Data.ByteString.Lazy as BSL
import Data.Binary.Get (ByteOffset)

import Test.Tasty
import Test.Tasty.QuickCheck



props :: TestTree
props = testGroup description [dummyTest] where
      description = "API consistency"
      dummyTest = localOption (QuickCheckTests 1)
                              (testProperty "(Static tests)" True)


      _typeInformation :: [N.TypeFormat]
      _typeInformation =
            [ N.Untyped, N.Hashed5, N.Hashed32, N.Hashed64, N.Shown, N.Full
            , D.Untyped, D.Hashed5, D.Hashed32, D.Hashed64, D.Shown, D.Full
            ]

      _typed :: (Typeable a, Binary a) => [TypeFormat -> a -> Typed a]
      _typed = [N.typed, D.typed]

      _erase :: [Typed a -> a]
      _erase = [N.erase, D.erase]

      _mapTyped :: Typeable b => [(a -> b) -> Typed a -> Typed b]
      _mapTyped  = [N.mapTyped, D.mapTyped]

      _reValue :: [(a -> a) -> Typed a -> Typed a]
      _reValue = [N.reValue, D.reValue]


      _reType :: Typeable a => [TypeFormat -> Typed a -> Typed a]
      _reType = [N.reType, D.reType]

      _preserialize :: [TypeInformation -> TypeInformation]
      _preserialize = [N.preserialize, D.preserialize]

      _encodeTyped :: (Typeable a, Binary a) => [TypeFormat -> a -> BSL.ByteString]
      _encodeTyped = [N.encodeTyped, D.encodeTyped]

      _decodeTyped :: (Typeable a, Binary a) => [BSL.ByteString -> Either String a]
      _decodeTyped = [N.decodeTyped, D.decodeTyped]

      _decodeTypedOrFail :: (Typeable a, Binary a) => [BSL.ByteString -> Either (BSL.ByteString, ByteOffset, String) (BSL.ByteString, ByteOffset, a)]
      _decodeTypedOrFail = [N.decodeTypedOrFail, D.decodeTypedOrFail]

      _unsafeDecodeTyped :: (Typeable a, Binary a) => [BSL.ByteString -> a]
      _unsafeDecodeTyped = [N.unsafeDecodeTyped, D.unsafeDecodeTyped]
