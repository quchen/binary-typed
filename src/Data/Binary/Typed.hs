-- | Defines a type-safe 'Data.Binary.Binary' instance to ensure data is
--   decoded with the type it was serialized from.

module Data.Binary.Typed (

      -- * How to use

      -- ** Motivation
      -- $motivation

      -- ** Practical example
      -- $usage




      -- * Construction, deconstruction
        Typed
      , typed
      , TypeFormat(..)
      , erase

      -- * Convenience API functions
      , encodeTyped
      , decodeTyped
      , decodeTypedOrFail
      , unsafeDecodeTyped
) where



import qualified Data.ByteString.Lazy as BSL

import           Data.Typeable (Typeable)

import           Data.Binary
import           Data.Binary.Get (ByteOffset)

import           Data.Binary.Typed.Internal



-- | Encode a 'Typeable' value to 'BSL.ByteString' that includes type
--   information.
encodeTyped :: (Typeable a, Binary a)
            => TypeFormat
            -> a
            -> BSL.ByteString
encodeTyped format value = encode (typed format value)



-- | Decode a typed value, throwing an error at runtime on failure.
--   Typed cousin of 'Data.Binary.decode'.
unsafeDecodeTyped :: (Typeable a, Binary a)
                  => BSL.ByteString
                  -> a
unsafeDecodeTyped = erase . decode



-- | Safely decode data, yielding 'Either' an error 'String' or the value,
--   along with meta-information of the consumed binary data.
--   Typed cousin of 'Data.Binary.decodeOrFail'.
decodeTypedOrFail :: (Typeable a, Binary a)
                  => BSL.ByteString
                  -> Either (BSL.ByteString, ByteOffset, String)
                            (BSL.ByteString, ByteOffset, a)
decodeTypedOrFail input = case decodeOrFail input of
      Right (rest, offset, value) -> Right (rest, offset, erase value)
      Left l -> Left l



-- | Safely decode data, yielding 'Either' an error 'String' or the value.
--   Equivalent to 'decodeTypedOrFail' stripped of the meta-information.
decodeTyped :: (Typeable a, Binary a)
            => BSL.ByteString
            -> Either String a
decodeTyped bs = case decodeTypedOrFail bs of
      Left  (_rest, _offset, err)   -> Left err
      Right (_rest, _offset, value) -> Right value




-- $motivation
--
-- Standard 'Binary' serializes to 'Data.ByteString.ByteString', which
-- is an untyped format; deserialization of unexpected input usually results
-- in unusable data.
--
-- This module defines a 'Typed' type, which allows serializing both a value
-- and the type of that value; deserialization can then check whether the
-- received data was sent assuming the right type, and error messages
-- may provide insight into the type mismatch.
--
-- Example without type safety:
--
-- @
-- test1 = let val = 10 :: 'Int'
--             enc = 'Data.Binary.encode' val
--             dec = 'Data.Binary.decode' enc :: 'Bool'
--         in  'print' dec
-- @
--
-- This behaves unexpectedly: An 'Int' value is converted to a 'Bool', which
-- corresponds to a wacky type coercion. The receiving end has no way of
-- knowing what the incoming data should have been interpreted as.
--
-- Using 'Typed', this can be avoided:
--
-- @
-- test2 = let val = 10 :: 'Int'
--             enc = 'Data.Binary.encode' ('typed' 'Full' val)
--             dec = 'Data.Binary.decode' enc :: 'Typed' 'Bool'
--         in  'print' dec
-- @
--
-- This time 'Data.Binary.decode' raises an error: the incoming data is tagged
-- as an 'Int', but is attempted to be decoded as 'Bool'.



-- $usage
--
-- For convenience, this module exports a couple of convenience functions that
-- have the type-mangling baked in already. The above example could have been
-- written as
--
-- @
-- test3 = let val = 10 :: 'Int'
--             enc = 'encodeTyped' val
--             dec = 'decodeTyped' enc :: 'Either' 'String' 'Bool'
--         in  'print' dec
-- @
