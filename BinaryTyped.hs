{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Defines a type-safe 'Binary' instance.
--
--   Standard 'Binary' serializes to 'BSL.ByteString', which
--   is an untyped format; deserialization of unexpected input usually results
--   in unusable data.
--
--   This module defines a 'Typed' type, which allows serializing both a value
--   and the type of that value; deserialization can then check whether the
--   received data was sent assuming the right type, and error messages
--   may provide insight into the type mismatch.
--
--   Example without type safety:
--
--   > test = let val = 'True'
--   >            enc = 'encode' val
--   >            dec = 'decode' enc :: 'Int'
--   >        in  print dec
--
--   This behaves unexpectedly: A 'True' value is converted to an 'Int', which
--   corresponds to a wacky type coercion. The receiving end has no way of
--   knowing what the incoming data should have been interpreted as.
--
--   Using 'Typed', this can be avoided:
--
--   > test' = let val = 'True'
--   >             enc = 'encode' ('typed' 'Full' val)
--   >             dec = 'decode' enc :: 'Typed' 'Int'
--   >         in  print dec
--
--   This time 'decode' raises an error: the incoming data is tagged as a
--   'Bool', but is attempted to be decoded as 'Int'.


module BinaryTyped (
      Typed

      -- * Construction
      , typed
      , TypeFormat(..)

      -- * Deconstruction
      , erase

      -- * Convenience API functions
      , encodeTyped
      , decodeTyped
      , decodeTypedOrFail
      , unsafeDecodeTyped
) where

import           GHC.Generics
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL

import           Data.Typeable (Typeable, typeOf)
import qualified Data.Typeable as Ty

import           Data.Binary
import           Data.Binary.Get (ByteOffset)

-- Crypto stuff for hashing
import qualified Crypto.Hash as Crypto
import           Data.Byteable (toBytes)



-- ^ Type information stored alongside a value to be serialized, so that the
--   recipient can do consistency checks. See 'TypeFormat' for more detailed
--   information on the fields.
data TypeInformation = HashedType BS.ByteString
                     | ShownType  String
                     | FullType   TypeRep
                     deriving (Eq, Ord, Show, Generic)

instance Binary TypeInformation



-- | A value suitable to be typechecked using the contained extra type
--   information.
data Typed a where
      Typed :: Typeable a => TypeInformation -> a -> Typed a

instance Show a => Show (Typed a) where
      show (Typed ty x) = "typed " ++ show format  ++ " (" ++ show x ++ ")"
            where format = case ty of HashedType {} -> Hashed
                                      ShownType  {} -> Shown
                                      FullType   {} -> Full

instance (Binary a, Typeable a) => Binary (Typed a) where
      get = do (ty, value) <- get
               let result = Typed ty value
               case typecheck result of
                     Left err -> fail   err -- "fail" is safe in Get Monad
                     Right _  -> return result
      put (Typed ty value) = put (ty, value)



-- | Different ways of including/verifying type information of serialized
--   messages.
data TypeFormat =

        -- | Compare types by their hash values, currently a 'MD5'
        --   representation of the 'Ty.TypeRep'.
        --
        --   * Requires only a handful of bytes per serialization.
        --   * Subject to false positive due to hash collisions, although in
        --     practice this should almost never happen.
        --   * Type errors cannot tell the expected type ("Expected X, received
        --     type with hash H")
        Hashed

        -- | Compare 'String' representation of types, obtained by calling
        --   'show' on the 'Ty.TypeRep'.
        --
        --   * Data size usually between 'Hashed' and 'Full'.
        --   * All types are unqualified, @Foo.X@ and @Bar.X@ look identical,
        --     thus type collisions (false positives) can happen.
        --   * Useful type errors ("expected X, received Y").
      | Shown

        -- | Compare the full representation of a data type.
        --
        --   * Much more verbose than hashes.
        --   * Correct comparison (no false positives).
        --   * Useful type errors ("expected X, received Y").
      | Full

      deriving (Eq, Ord, Show)



-- | Construct a 'Typed' value using the chosen type format.
typed :: Typeable a => TypeFormat -> a -> Typed a
typed format x = Typed typeInformation x where
      ty = typeOf x
      typeInformation = case format of
            Hashed -> HashedType (typeHash ty)
            Shown  -> ShownType  (show     ty)
            Full   -> FullType   (getFull  ty)



-- | Extract the value of a 'Typed'.
--
--   The well-typedness of this is ensured by the 'typed' smart constructor and
--   the 'Binary' instance of 'Typed'.
erase :: Typed a -> a
erase (Typed _ x) = x



-- | Typecheck a 'Typed'. The result is either the contained value, or an
--   error message.
typecheck :: Typed a -> Either String a
typecheck (Typed typeInformation x) = case typeInformation of
      HashedType hash
            | expectedHash == hash -> Right x
            | otherwise            -> Left (hashErrorMsg hash)
      ShownType str
            | expectedShow == str  -> Right x
            | otherwise            -> Left (shownErrorMsg str)
      FullType full
            | expectedFull == full -> Right x
            | otherwise            -> Left (fullErrorMsg full)


      where expectedType = typeOf x
            expectedHash = typeHash expectedType
            expectedShow = show     expectedType
            expectedFull = getFull  expectedType

            hashErrorMsg hash = unwords [ "Type error: expected type"
                                        , expectedShow
                                        , "with hash"
                                        , show expectedHash ++ ","
                                        , "but received data with hash"
                                        , show hash
                                        ]
            shownErrorMsg str = unwords [ "Type error: expected type"
                                        , expectedShow ++ ","
                                        , "but received data with type"
                                        , str
                                        ]
            fullErrorMsg full = unwords [ "Type error: expected type"
                                        , expectedShow ++ ","
                                        , "but received data with type"
                                        , show full
                                        ]



-- | Hash a 'Ty.TypeRep'.
typeHash :: Ty.TypeRep -> BS.ByteString
typeHash = md5 . show where

      md5 = toBytes . hash . encode

      hash :: BSL.ByteString -> Crypto.Digest Crypto.MD5
      hash = Crypto.hashlazy



-- | 'Ty.TypeRep' without the (internal) fingerprint
data TypeRep = TypeRep TyCon [TypeRep]
      deriving (Eq, Ord, Show, Generic)
instance Binary TypeRep



-- | 'Ty.TyCon' without the (internal) fingerprint
data TyCon = TyCon String -- Package
                   String -- Module
                   String -- Name
      deriving (Eq, Ord, Show, Generic)
instance Binary TyCon



-- | Get (only) the representation of a data type, i.e. strip all hashes.
getFull :: Ty.TypeRep -> TypeRep
getFull typerep = let (tycon, args) = Ty.splitTyConApp typerep
                  in  TypeRep (stripFP tycon) (map getFull args)
      where -- TyCon without fingerprint.
            stripFP :: Ty.TyCon -> TyCon
            stripFP tycon = TyCon (Ty.tyConPackage tycon)
                                  (Ty.tyConModule  tycon)
                                  (Ty.tyConName    tycon)
                                  -- The Typeable API doesn't expose the
                                  -- TyCon constructor, so pattern matching
                                  -- is not possible here (without depending
                                  -- on Typeable.Internal).





-- #############################################################################
-- ###  Convenience API functions  #############################################
-- #############################################################################



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