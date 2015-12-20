{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}





-- | Defines a type-safe 'Data.Binary.Binary' instance to ensure data is
--   encoded with the type it was serialized from.
--
--  * The "Data.Binary.Typed.Tutorial" provides some more examples of usage.
--  * The "Data.Binary.Typed.Debug" is useful to ensure calculated type
--    representations are shared properly.
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

      -- ** Decoding
      , decodeTyped
      , decodeTypedOrFail
      , unsafeDecodeTyped

) where



import qualified Data.ByteString.Lazy as BSL

import           Data.Typeable (Typeable, typeRep, Proxy(..))

import           Data.Binary
import           Data.Binary.Get (ByteOffset)

import           Data.Binary.Typed.Internal





-- | Modify the value contained in a 'Typed', keeping the same sort of type
-- representation. In other words, calling 'mapTyped' on something that is
-- typed using 'Hashed' will yield a 'Hashed' value again.
--
-- Note: this destroys 'preserialize'd information, so that values have to be
-- 'preserialize'd again if desired. As a consequence, @'mapTyped' 'id'@
-- can be used to un-'preserialize' values.
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
encodeTyped :: forall a.
               (Typeable a, Binary a)
            => TypeFormat
            -> a
            -> BSL.ByteString
encodeTyped format = \x -> encode (Typed typeInfo x)
      where typeInfo = preserialize (makeTypeInformation format typerep)
            typerep = typeRep (Proxy :: Proxy a)

{-# INLINE encodeTyped #-}




-- | Decode a typed value, throwing a descriptive 'error' at runtime on failure.
-- Typed cousin of 'Data.Binary.decode'. Based on 'decodeTypedOrFail'.
--
-- @
-- encoded = 'encodeTyped' 'Full' ("hello", 1 :: 'Int', 2.34 :: 'Double')
--
-- -- \<value\>
-- 'unsafeDecodeTyped' encoded :: ('String', 'Int', 'Double')
--
-- -- (Descriptive) runtime 'error'
-- 'unsafeDecodeTyped' encoded :: ('Char', 'Int', 'Double')
-- @
unsafeDecodeTyped :: (Typeable a, Binary a)
                  => BSL.ByteString
                  -> a
unsafeDecodeTyped = \x -> case decodeTypedOrFail x of
      Left  (_,_,err)   -> error ("unsafeDecodeTyped' failure: " ++ err)
      Right (_,_,value) -> value

{-# INLINE unsafeDecodeTyped #-} -- Inlining is crucial for caching to work!



-- | Safely decode data, yielding 'Either' an error 'String' or the value.
-- Equivalent to 'decodeTypedOrFail' stripped of the non-essential data.
-- Based on 'decodeTypedOrFail'.
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
decodeTyped = \x -> case decodeTypedOrFail x of
      Left  (_,_,err)   -> Left err
      Right (_,_,value) -> Right value

{-# INLINE decodeTyped #-} -- Inlining is crucial for caching to work!



-- | Safely decode data, yielding 'Either' an error 'String' or the value,
-- along with meta-information of the consumed binary data.
--
-- * Typed cousin of 'Data.Binary.decodeOrFail'.
--
-- * Like 'decodeTyped', but with additional data.
--
-- * Automatically caches 'Hashed5', 'Hashed32' and 'Hashed64' representations,
--   so that typechecking does not need to recalculate them on every decoding.
decodeTypedOrFail :: forall a.
                     (Typeable a, Binary a)
                  => BSL.ByteString
                  -> Either (BSL.ByteString, ByteOffset, String)
                            (BSL.ByteString, ByteOffset, a)
decodeTypedOrFail = \input -> do
      (rest, offset, typed'@(Typed' ty value)) <- decodeOrFail input
      let addMeta x = (rest, offset, x)
      if isCached ty
            then Right (addMeta value) -- cache hit, don't typecheck
            else case typecheck' typed' of -- cache miss, typecheck manually
                  Left err -> Left  (addMeta err)
                  Right _  -> Right (addMeta value)

      where

      exTypeRep = typeRep (Proxy :: Proxy a)
      cache = map (\format -> makeTypeInformation format exTypeRep)
                  [Hashed5, Hashed32, Hashed64] -- List of formats to be cached
      isCached = (`elem` cache)

{-# INLINE decodeTypedOrFail #-} -- Inlining is crucial for caching to work!
