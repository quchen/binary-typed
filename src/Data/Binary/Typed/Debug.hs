{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | This module has the same interface as "Data.Binary.Typed", but emits
-- debugging messages via "Debug.Trace" whenever a 'TypeInformation' is
-- calculated. This is useful to determine whether caching works properly,
-- i.e. if a single serialization point emits a lot of caching messages
-- it's worth having a look at.
--
-- A simple example to check sharing is to evaluate
--
-- @
-- 'map' ('encodeTyped' 'Hashed5') "hello world!"
-- @
--
-- This should print only one debug message "TypeRep/Hashed5 calculated",
-- since the encoding function is shared between all invocations.



module Data.Binary.Typed.Debug (

      -- * Core functions
        Normal.Typed
      , typed
      , Normal.TypeFormat(..)
      , Internal.erase


      -- * Useful general helpers
      , Normal.mapTyped
      , Normal.reValue
      , reType
      , Internal.preserialize


      -- * Typed serialization

      -- ** Encoding
      , encodeTyped

      -- ** Decoding
      , decodeTyped
      , decodeTypedOrFail
      , unsafeDecodeTyped

) where



import qualified Data.ByteString.Lazy as BSL

import           Data.Typeable (Typeable, typeRep, Proxy(..), typeOf)
import qualified Data.Typeable as Ty

import           Data.Binary
import           Data.Binary.Get (ByteOffset)

import qualified Data.Binary.Typed as Normal
import           Data.Binary.Typed.Internal as Internal hiding (makeTypeInformation)
import qualified Data.Binary.Typed.Internal as Internal (makeTypeInformation)

import qualified Debug.Trace as Debug



-- | Similar to 'makeTypeInformation', but prints a message each time it's
--   forced.
makeTypeInformationDebug :: TypeFormat -> Ty.TypeRep -> TypeInformation
makeTypeInformationDebug format typerep =
      let message = "TypeRep/" ++ show format ++ " calculated"
      in  Debug.trace message (Internal.makeTypeInformation format typerep)



-- | Change the way a type is represented inside a 'Typed' value.
--
-- @
-- 'reType' format x = 'typed' format ('erase' x)
-- @
reType :: Typeable a => TypeFormat -> Typed a -> Typed a
reType format (Typed _ty x) = Typed (makeTypeInformationDebug format (typeOf x)) x

















-- ##########################################################################
-- ###                                                                    ###
-- ###  What follows was simply copied from the normal module, replacing  ###
-- ###  makeTypeInformation with makeTypeInformationDebug.                ###
-- ###                                                                    ###
-- ##########################################################################



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
      where typeInfo = preserialize (makeTypeInformationDebug format typerep)
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
      cache = map (\format -> makeTypeInformationDebug format exTypeRep)
                  [Hashed5, Hashed32, Hashed64] -- List of formats to be cached
      isCached = (`elem` cache)

{-# INLINE decodeTypedOrFail #-} -- Inlining is crucial for caching to work!
