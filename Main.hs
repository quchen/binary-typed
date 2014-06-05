{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Defines a type-safe 'Binary' instance.
--
--   Standard 'Binary' serializes to 'Data.ByteString.Lazy.ByteString', which
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


module Mainn (
      Typed

      -- * Construction
      , typed
      , TypeFormat(..)

      -- * Deconstruction
      , erase
) where

import GHC.Generics
import Data.Typeable (Typeable, typeOf)
import qualified Data.Typeable as Ty
import qualified Data.Typeable.Internal as TI (Fingerprint(..), TypeRep(..))
import Data.Binary
import Control.Applicative



-- ^ Type information stored alongside a value to be serialized, so that the
--   recipient can do consistency checks. See 'TypeFormat' for more detailed
--   information on the fields.
data TypeInformation = HashedType Fingerprint
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
      get = do result <- get
               case typecheck result of
                     Left err -> fail   err
                     Right _  -> return result
      put (Typed ty value) = put (ty, value)



-- | Determine how the 'TypeInformation' should be created by 'typed'.
data TypeFormat =

        -- | Compare types by their hash values.
        --
        --   * Requires only 8 bytes per serialization.
        --   * Subject to false positive due to hash collisions, although in
        --     practice this should almost never happen
        --     (see "GHC.Fingerprint.Type" for information on hashing).
        --   * Type errors cannot tell the expected type ("Expected X, received
        --     type with hash H")
        --   * Depends on "Data.Typeable.Internal", so the hash function is
        --     subject to sudden breaking changes. Not recommended for
        --     future-proof communication.
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
            Hashed -> HashedType (getFingerprint ty)
            Shown  -> ShownType  (show           ty)
            Full   -> FullType   (getFull        ty)



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
            | otherwise            -> Left "type error (hash)"
      ShownType str
            | expectedShow == str  -> Right x
            | otherwise            -> Left "type error (shown)"
      FullType full
            | expectedFull == full -> Right x
            | otherwise            -> Left "type error (full)"


      where expectedType = typeOf x
            expectedHash = getFingerprint expectedType
            expectedShow = show           expectedType
            expectedFull = getFull        expectedType



-- | Extract the 'TI.Fingerptint' hash from a 'TI.TypeRep'.
getFingerprint :: TI.TypeRep -> Fingerprint
getFingerprint (TI.TypeRep fp _tycon _args) = Fingerprint fp



-- | 'Ty.TypeRep' without the 'TI.Fingerprint'.
data TypeRep = TypeRep TyCon [TypeRep]
      deriving (Eq, Ord, Show, Generic)
instance Binary TypeRep



-- | 'Ty.TyCon' without the 'TI.Fingerprint'.
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



-- | Wrapper around 'TI.Fingerprint' to avoid orphan 'Binary' instance
newtype Fingerprint = Fingerprint TI.Fingerprint
      deriving (Eq, Ord, Show)

instance Binary Fingerprint where
      get = liftA2 (\a b -> Fingerprint (TI.Fingerprint a b)) get get
      put (Fingerprint (TI.Fingerprint a b)) = put a *> put b
