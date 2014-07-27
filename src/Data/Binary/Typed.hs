-- | Defines a type-safe 'Data.Binary.Binary' instance to ensure data is
--   decoded with the type it was serialized from.
--
--   For usage information, see the "Data.Binary.Typed.Tutorial" module.

module Data.Binary.Typed (

      -- * Core functions
        Typed
      , typed
      , TypeFormat(..)
      , erase


      -- * Useful general helpers
      , mapTyped
      , reValue
      , reType
      , preserialize


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

import           Data.Typeable (Typeable, typeOf)

import           Data.Binary
import           Data.Binary.Get (ByteOffset)

import           Data.Binary.Typed.Internal




-- | Modify the value contained in a 'Typed', keeping the same sort of type
-- representation. In other words, calling 'mapTyped' on something that is
-- typed using 'Hashed' will yield a 'Hashed' value again.
--
-- Note: this destroys 'precache'd information, so that values have to be
-- 'precache'd again if desired. As a consequence, @'mapTyped' 'id'@
-- can be used to un-'precache' values.
mapTyped :: Typeable b => (a -> b) -> Typed a -> Typed b
mapTyped f (Typed ty x) = typed (getFormat ty) (f x)



-- | Change the value contained in a 'Typed', leaving the type representation
-- unchanged. This can be useful to avoid recomputation of the included type
-- information, and can improve performance significantly if many individual
-- messages are serialized.
--
-- Can be seen as a more efficient 'mapTyped' in case @f@ is an endomorphism
-- (i.e. has type @a -> a@).
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
-- information. This function is useful to create specialized typed encoding
-- functions, because the type information is cached and does not need to be
-- recalculated on every serialization.
--
-- Observationally, @'encodeTyped' format value@ is equivalent to
-- @'encode' ('typed' format value)@. However, 'encodeTyped' does the type
-- information related calculations in advance and shares the results between
-- future invocations of it, making it much more efficient to serialize many
-- values of the same type.
encodeTyped :: (Typeable a, Binary a)
            => TypeFormat
            -> a
            -> BSL.ByteString
encodeTyped format = \x ->
      let ty = preserialize (makeTypeInformation format (typeOf x))
      in  encode (Typed ty x)



encodeTypedLike ::
         (Typeable a, Binary a)
      => Typed a
      -> a
      -> BSL.ByteString
encodeTypedLike (Typed ty _) = encodeTyped (getFormat ty)

{-# DEPRECATED encodeTypedLike
               "'encodeTyped' now caches automatically for all types" #-}



-- | Decode a typed value, throwing an error at runtime on failure.
-- Typed cousin of 'Data.Binary.decode'.
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
--
unsafeDecodeTyped :: (Typeable a, Binary a)
                  => BSL.ByteString
                  -> a
unsafeDecodeTyped = erase . decode



-- | Safely decode data, yielding 'Either' an error 'String' or the value,
-- along with meta-information of the consumed binary data.
--
-- * Typed cousin of 'Data.Binary.decodeOrFail'.
--
-- * Like 'decodeTyped', but with additional data.
--
decodeTypedOrFail :: (Typeable a, Binary a)
                  => BSL.ByteString
                  -> Either (BSL.ByteString, ByteOffset, String)
                            (BSL.ByteString, ByteOffset, a)
decodeTypedOrFail input = case decodeOrFail input of
      Right (rest, offset, value) -> Right (rest, offset, erase value)
      Left l -> Left l



-- | Safely decode data, yielding 'Either' an error 'String' or the value.
-- Equivalent to 'decodeTypedOrFail' stripped of the non-essential data.
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
--
decodeTyped :: (Typeable a, Binary a)
            => BSL.ByteString
            -> Either String a
decodeTyped bs = case decodeTypedOrFail bs of
      Left  (_rest, _offset, err)   -> Left err
      Right (_rest, _offset, value) -> Right value
