-- | Defines a type-safe 'Data.Binary.Binary' instance to ensure data is
--   decoded with the type it was serialized from.

module Data.Binary.Typed (

      -- * How to use

      -- ** Motivation
      -- $motivation

      -- ** Usage
      -- $usage

      -- ** Brief overview
      -- $overview




      -- * Core functions
        Typed
      , typed
      , TypeFormat(..)
      , erase


      -- * Useful general helpers
      , mapTyped
      , reValue
      , reType
      , precache


      -- * Typed serialization

      -- ** Encoding
      , encodeTyped
      , encodeTypedLike

      -- ** Decoding
      , decodeTyped
      , decodeTypedOrFail
      , unsafeDecodeTyped

) where



import qualified Data.ByteString.Lazy as BSL

import           Data.Typeable (Typeable)

import           Data.Binary
import           Data.Binary.Get (ByteOffset)

import           Data.Binary.Typed.Internal




-- | Modify the value contained in a 'Typed', keeping the same sort of type
--   representation. In other words, calling 'mapTyped' on something that is
--   typed using 'Hashed' will yield a 'Hashed' value again.
--
--   Note: this destroys 'precache'd information, so that values have to be
--   'precache'd again if desired. As a consequence, @'mapTyped' 'id'@
--   can be used to un-'precache' values.
mapTyped :: Typeable b => (a -> b) -> Typed a -> Typed b
mapTyped f (Typed ty x) = typed (getFormat ty) (f x)



-- | Change the value contained in a 'Typed', leaving the type representation
--   unchanged. This can be useful to avoid recomputation of the included type
--   information, and can improve performance significantly if many individual
--   messages are serialized.
--
--   Can be seen as a more efficient 'mapTyped' in case @f@ is an endomorphism
--   (i.e. has type @a -> a@).
reValue :: (a -> a) -> Typed a -> Typed a
reValue f (Typed ty x) = Typed ty (f x)



-- | Change the way a type is represented inside a 'Typed' value.
--
-- @
-- 'reType' format x = 'typed' format ('erase' x)
-- @
reType :: Typeable a => TypeFormat -> Typed a -> Typed a
reType format (Typed _ty x) = typed format x



-- | Encode a 'Typeable' value to 'BSL.ByteString' that includes type
-- information. If at all possible, prefer the more efficient 'encodeTypedLike'
-- though.
--
-- @
-- 'encodeTyped' format value = 'encode' ('typed' format value)
-- @
encodeTyped :: (Typeable a, Binary a)
            => TypeFormat
            -> a
            -> BSL.ByteString
encodeTyped format value = encode (typed format value)



-- | Version of 'encodeTyped' that avoids recomputing the type representation
--   of the input by using the one already contained in the first parameter.
--   This is usually /much/ more efficient than using 'encode', having a
--   computational cost similar to using 'Binary' directly.
--
-- @
-- 'encodeTypedLike' ty x
-- -- is observationally identical to
-- 'encode' ('reValue' ('const' x) ty)
-- @
--
-- This function is intended to generate new encoding functions like so:
--
-- @
-- encodeInt :: 'Int' -> 'Data.ByteString.Lazy.ByteString'
-- encodeInt = 'encodeTypedLike' ('typed' 'Full' 0)
-- @
encodeTypedLike
      :: (Typeable a, Binary a)
      => Typed a
      -> a
      -> BSL.ByteString
encodeTypedLike dummy = let (Typed ty _) = precache dummy
                        in  encode . Typed ty



-- | Decode a typed value, throwing an error at runtime on failure.
--   Typed cousin of 'Data.Binary.decode'.
--
-- @
-- encoded = 'encodeTyped' 'Full' ("hello", 1 :: 'Int', 2.34 :: 'Double')
--
-- -- \<value\>
-- 'unsafeDecodeTyped' encoded :: ('String', 'Int', 'Double')
--
-- -- (Descriptive) runtime error
-- 'unsafeDecodeTyped' encoded :: ('Char', 'Int', 'Double')
-- @
unsafeDecodeTyped :: (Typeable a, Binary a)
                  => BSL.ByteString
                  -> a
unsafeDecodeTyped = erase . decode



-- | Safely decode data, yielding 'Either' an error 'String' or the value,
--   along with meta-information of the consumed binary data.
--
--   * Typed cousin of 'Data.Binary.decodeOrFail'.
--   * Like 'decodeTyped', but with additional data.
decodeTypedOrFail :: (Typeable a, Binary a)
                  => BSL.ByteString
                  -> Either (BSL.ByteString, ByteOffset, String)
                            (BSL.ByteString, ByteOffset, a)
decodeTypedOrFail input = case decodeOrFail input of
      Right (rest, offset, value) -> Right (rest, offset, erase value)
      Left l -> Left l



-- | Safely decode data, yielding 'Either' an error 'String' or the value.
--   Equivalent to 'decodeTypedOrFail' stripped of the non-essential data.
--
-- @
-- encoded = 'encodeTyped' 'Full' ("hello", 1 :: 'Int', 2.34 :: 'Double')
--
-- -- Right \<value\>:
-- 'decodeTyped' encoded :: 'Either' 'String' ('String', 'Int', 'Double')
--
-- -- Left "Type error: expected (Char, Int, Double), got (String, Int, Double)"
-- 'decodeTyped' encoded :: 'Either' 'String' ('Char', 'Int', 'Double')
-- @
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
-- For example, this uses 'Data.Binary.Binary' directly:
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
--
-- However, using 'encodeTyped' is computationally inefficient when many
-- messages of the same type are serialized, since it recomputes a serialized
-- version of that type for every single serialized value from scratch.
-- 'encodeTypedLike' exists to remedy that: it takes a separately constructed
-- 'Typed' dummy value, and computes a new serialization function for that type
-- out of it. This serialization function then re-uses the type representation
-- of the dummy value, and simply replaces the contained value on each
-- serialization so that no unnecessary overhead is introduced.
--
-- @
-- -- Computes 'Int's type representation 100 times:
-- manyIntsNaive = map 'encodeTyped' [1..100 :: 'Int']
--
-- -- Much more efficient: prepare dummy value to precache the
-- -- type representation, computing it only once:
-- 'encodeInt' = 'encodeTypedLike' ('typed' 'Full' (0 :: 'Int'))
-- manyIntsCached = map 'encodeInt' [1..100]
-- @

-- $overview
--
-- The core definitions are:
--
--   * 'Typed' (the main type)
--   * 'typed' (construct 'Typed' values)
--   * 'TypeFormat' (a helper type for 'typed')
--   * 'erase' (deconstruct 'Typed' vales)
--
-- In addition to those, a couple of useful helper functions with more efficient
-- implementation than what the core definitions could offer:
--
--   * 'mapTyped' (change values contained in 'Typed's)
--   * 'reValue' (change value, but don't recompute type representation)
--   * 'reType' (change type representation, but keep value)
--   * 'precache' (compute serialized type representation, useful as an optimization)
--
-- Lastly, there are a number of encoding/decoding functions, mostly for
-- convenience:
--
--   * 'encodeTyped' (pack in 'Typed' and then 'encode')
--   * 'encodeTypedLike' (usually much more efficient version of 'encodeTyped')
--   * 'decodeTyped' (decode 'Typed' 'Data.ByteString.Lazy.ByteString' to @'Either' 'String' a@)
--   * 'decodeTypedOrFail' (like 'decodeTyped', but with more meta information)
--   * 'unsafeDecodeTyped' (which throws a runtime error on type mismatch)
