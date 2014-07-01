{-# OPTIONS_GHC -fno-warn-unused-imports #-}



-- | This meta-module exists only for documentational purposes; the library
--   functionality is found in "Data.Binary.Typed".



module Data.Binary.Typed.Tutorial (

      -- * Motivation
      -- $motivation

      -- * Basic usage
      -- $usage

      -- * API overview
      -- $api

) where



import Data.Binary.Typed
import Data.Binary.Typed.Internal

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





-- $api
--
--
-- The core definitions in "Data.Binary.Typed" are:
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
