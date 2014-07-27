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
-- This package is typically used for debugging purposes. 'Hashed32' type
-- information keeps the size overhead relatively low, but requires a certain
-- amount of computational ressources. It is reliable at detecting errors, but
-- not very good at telling specifics about it. If a problem is identified, the
-- typing level can be increased to 'Shown' or 'Full', providing information
-- about the involved types. If performance is critical, 'Untyped' \"typed\"
-- encoding can be used, with minimal overhead compared to using 'Binary'
-- directly.
--
-- For convenience, this module exports a couple of convenience functions that
-- have the type-mangling baked in already. The above example could have been
-- written as
--
-- @
-- test3 = let val = 10 :: 'Int'
--             enc = 'encodeTyped' 'Hashed32' val
--             dec = 'decodeTyped' enc :: 'Either' 'String' 'Bool'
--         in  'print' dec
-- @
--
-- Using 'encodeTyped' in particular has a significant advantage: when used to
-- create new specialized encoding functions, the type information has to be
-- calculated only once, and can be shared among further invocations of the
-- function. In other words, using
--
-- @
-- encodeInt :: 'Int' -> 'Data.ByteString.Lazy.ByteString'
-- encodeInt = 'encodeTyped' 'Hashed32'
-- @
--
-- is much more efficient than
--
-- @
-- encodeInt :: 'Int' -> 'Data.ByteString.Lazy.ByteString'
-- encodeInt = 'encode' . 'typed' 'Hashed32'
-- @
--
-- since the latter recalculates the hash of \"Int\" on every invocation.





-- $api
--
--
-- The core definitions in "Data.Binary.Typed" are:
--
--   * 'Typed' (the main type)
--
--   * 'typed' (construct 'Typed' values)
--
--   * 'TypeFormat' (a helper type for 'typed')
--
--   * 'erase' (deconstruct 'Typed' vales)
--
-- In addition to those, a couple of useful helper functions with more efficient
-- implementation than what the core definitions could offer:
--
--   * 'mapTyped' (change values contained in 'Typed's)
--
--   * 'reValue' (change value, but don't recompute type representation)
--
--   * 'reType' (change type representation, but keep value)
--
--   * 'preserialize' (compute serialized type representation and cache it, useful as an optimization)
--
-- Lastly, there are a number of encoding/decoding functions:
--
--   * 'encodeTyped' (pack in 'Typed' and then 'encode', but more efficient)
--
--   * 'decodeTyped' (decode 'Typed' 'Data.ByteString.Lazy.ByteString' to @'Either' 'String' a@)
--
--   * 'decodeTypedOrFail' (like 'decodeTyped', but with more meta information)
--
--   * 'unsafeDecodeTyped' (which throws a runtime error on type mismatch)
