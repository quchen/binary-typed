{-# LANGUAGE DeriveGeneric #-}



-- | Internals, exposed mostly for potential use by the testsuites.
--
-- __Not recommended to be used from within other independent libraries.__
module Data.Binary.Typed.Internal (

      -- * 'Typed'
        Typed(..)
      , TypeInformation(..)
      , Hash(..)
      , typed
      , TypeFormat(..)
      , getFormat
      , typecheck
      , erase

      -- * 'TypeRep'
      , TypeRep(..)
      , stripTypeRep
      , unStripTypeRep
      , hashType

      -- * 'TyCon'
      , TyCon(..)
      , stripTyCon
      , unStripTyCon

) where


import           GHC.Generics
import           Control.Monad
import           Data.Monoid
import           Numeric (showHex)
-- import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL

import           Data.Typeable (Typeable, typeOf)
import qualified Data.Typeable as Ty

import           Data.Binary
import           Data.Binary.Get (getLazyByteString)
import           Data.Binary.Put (putLazyByteString)

-- Crypto stuff for hashing
import qualified Crypto.Hash as Crypto
import           Data.Byteable (toBytes)



-- ^ Type information stored alongside a value to be serialized, so that the
--   recipient can do consistency checks. See 'TypeFormat' for more detailed
--   information on the fields.
data TypeInformation = NoType
                     | HashedType Hash
                     | ShownType  Hash String
                     | FullType   TypeRep
                     deriving (Eq, Ord, Show, Generic)

instance Binary TypeInformation



-- | Extract which 'TypeFormat' was used to create a certain 'TypeInformation'.
getFormat :: TypeInformation -> TypeFormat
getFormat (NoType     {}) = Untyped
getFormat (HashedType {}) = Hashed
getFormat (ShownType  {}) = Shown
getFormat (FullType   {}) = Full



-- | A hash value of a 'TypeRep'. Currently a 32-bit 'BSL.ByteString'.
data Hash = Hash BSL.ByteString
      deriving (Eq, Ord)

instance Show Hash where
      show (Hash hash) = (BSL.foldr (\x xs -> showHex x . xs) id hash) ""

-- Manual instance of a fixed-size ByteString type (avoids sending the length)
instance Binary Hash where
      put (Hash hash) = putLazyByteString hash >> replicateM_ pad (putWord8 0)
            where pad = fromIntegral (32 - BSL.length hash)
      get = fmap Hash (getLazyByteString 32)



-- | A value suitable to be typechecked using the contained extra type
--   information.
data Typed a = Typed TypeInformation a
      -- ^ Using this data constructor directly is unsafe, as it allows
      -- construction of ill-typed 'Typed' data. Use the 'typed' smart
      -- constructor unless you really need 'Typed'.

instance Show a => Show (Typed a) where
      show (Typed ty x) = "typed " ++ show format  ++ " (" ++ show x ++ ")"
            where format = case ty of NoType     {} -> Untyped
                                      HashedType {} -> Hashed
                                      ShownType  {} -> Shown
                                      FullType   {} -> Full

-- | Ensures data is decoded as the appropriate type with high or total
--   confidence (depending on with what 'TypeFormat' the 'Typed' was
--   constructed).
instance (Binary a, Typeable a) => Binary (Typed a) where
      get = do (ty, value) <- get
               either fail return (typecheck (Typed ty value))
               -- NB: 'fail' is safe in Get Monad
      put (Typed ty value) = put (ty, value)



-- | Different ways of including/verifying type information of serialized
--   messages.
data TypeFormat =

        -- | Include no type information.
        --
        --   * Requires one byte more than using 'Binary' directly (namely to
        --     tag the data as untyped).
        Untyped

        -- | Compare types by their hash values, currently an 'MD5'-based
        --   representation of the 'Ty.TypeRep'.
        --
        --   * Requires only a handful of bytes per serialization.
        --   * Subject to false positive due to hash collisions, although in
        --     practice this should almost never happen.
        --   * Type errors cannot tell the provided type ("Expected X, received
        --     type with hash H")
      | Hashed

        -- | Compare 'String' representation of types, obtained by calling
        --   'show' on the 'TypeRep', and also include a hash value
        --   (like 'Hashed'). The former is mostly for readable error messages,
        --   the latter provides collision resistance.
        --
        --   * Data size larger than 'Hashed', but usually smaller than 'Full'.
        --   * Both the hash and the shown type must match to satisfy the
        --     typechecker.
        --   * Useful type errors ("expected X, received Y"). All types are
        --     unqualified though, making @Foo.X@ and @Bar.X@ look identical in
        --     error messages.
      | Shown

        -- | Compare the full representation of a data type.
        --
        --   * More verbose than 'Hashed' and 'Shown'. As a rule of thumb,
        --     transmitted data is roughly the same as 'Shown', but all names
        --     are fully qualified (package, module, type name).
        --   * Correct comparison (no false positives). An semi-exception here
        --     is when types change between package versions:
        --     @package-1.0 Foo.X@ and @package-1.1 Foo.X@ count as the same
        --     type.
        --   * Useful type errors ("expected X, received Y"). All types are
        --     unqualified though, making @Foo.X@ and @Bar.X@ look identical in
        --     error messages.
      | Full

      deriving (Eq, Ord, Show)



-- | Construct a 'Typed' value using the chosen type format.
--
-- Example:
--
-- @
-- value = 'typed' 'Full' ("hello", 1 :: 'Int', 2.34 :: 'Double')
-- encded = 'encode' value
-- @
--
-- The decode site can now verify whether decoding happens with the right type.
typed :: Typeable a => TypeFormat -> a -> Typed a
typed format x = Typed typeInformation x where
      ty = typeOf x
      typeInformation = case format of
            Untyped -> NoType
            Hashed  -> HashedType (hashType     ty)
            Shown   -> ShownType  (hashType     ty) (show ty)
            Full    -> FullType   (stripTypeRep ty)



-- | Extract the value of a 'Typed', i.e. strip off the explicit type
-- information.
--
-- This function is safe to use for all 'Typed' values created by the public
-- API, since all construction sites ensure the actual type matches the
-- contained type description.
--
-- @
-- 'erase' ('typed' format x) == x
-- @
erase :: Typed a -> a
erase (Typed _ty value) = value



-- | Typecheck a 'Typed'. Returns the input if the types work out, and an error
--   message otherwise.

typecheck :: Typeable a => Typed a -> Either String (Typed a)
typecheck ty@(Typed typeInformation x) = case typeInformation of
      HashedType hash    | exHash /= hash -> Left (hashErrorMsg hash)
      FullType full      | exFull /= full -> Left (fullErrorMsg full)
      ShownType hash str | (exHash, exShow) /= (hash, str)
                                          -> Left (shownErrorMsg hash str)
      _no_type_error -> Right ty


      where

      -- ex = expected
      exType = typeOf x
      exHash = hashType     exType
      exShow = show         exType
      exFull = stripTypeRep exType

      hashErrorMsg hash = unwords [ "Type error: expected type"
                                  , exShow
                                  , "with hash"
                                  , show exHash ++ ","
                                  , "but received data with hash"
                                  , show hash
                                  ]
      shownErrorMsg hash str = unwords
                                  [ "Type error: expected type"
                                  , exShow
                                  , "with hash"
                                  , show exHash ++ ","
                                  , "but received data with type"
                                  , str
                                  , "and hash"
                                  , show hash
                                  ]
      fullErrorMsg full = unwords [ "Type error: expected type"
                                  , exShow ++ ","
                                  , "but received data with type"
                                  , show full
                                  ]



-- | Hash a 'Ty.TypeRep'.
hashType :: Ty.TypeRep -> Hash
hashType = Hash
         . pad 32 -- Length of the hash
         . BSL.fromStrict
         . toBytes
         . hash
         . encode
         . stripTypeRep

      where

      hash :: BSL.ByteString -> Crypto.Digest Crypto.MD5
      hash = Crypto.hashlazy

      -- Pad a ByteString to a certain length by *prepending* zeros or
      -- shrink it by truncating.
      pad l bs = BSL.take l (BSL.replicate (l - BSL.length bs) 0 <> bs)



-- | 'Ty.TypeRep' without the (internal) fingerprint.
data TypeRep = TypeRep TyCon [TypeRep]
      deriving (Eq, Ord, Generic)
instance Binary TypeRep

instance Show TypeRep where
      show = show . unStripTypeRep



-- | 'Ty.TyCon' without the (internal) fingerprint.
data TyCon = TyCon String String String -- ^ Package, module, constructor name
      deriving (Eq, Ord, Generic)
instance Binary TyCon

instance Show TyCon where
      show = show . unStripTyCon



-- | Strip a 'Ty.TypeRep' off the fingerprint. Inverse of 'unStripTypeRep'.
stripTypeRep :: Ty.TypeRep -> TypeRep
stripTypeRep typerep = TypeRep (stripTyCon tycon) (map stripTypeRep args)
      where (tycon, args) = Ty.splitTyConApp typerep



-- | Add a fingerprint to a 'TypeRep'. Inverse of 'stripTypeRep'.
unStripTypeRep :: TypeRep -> Ty.TypeRep
unStripTypeRep (TypeRep tyCon args) = Ty.mkTyConApp (unStripTyCon tyCon)
                                                    (map unStripTypeRep args)



-- | Strip a 'Ty.TyCon' off the fingerprint. Inverse of 'unStripTyCon'.
stripTyCon :: Ty.TyCon -> TyCon
stripTyCon tycon = TyCon (Ty.tyConPackage tycon)
                         (Ty.tyConModule  tycon)
                         (Ty.tyConName    tycon)
                         -- The Typeable API doesn't expose the
                         -- TyCon constructor, so pattern matching
                         -- is not possible here (without depending
                         -- on Typeable.Internal).



-- | Add a fingerprint to a 'TyCon'. Inverse of 'stripTyCon'.
unStripTyCon :: TyCon -> Ty.TyCon
unStripTyCon (TyCon p m n) = Ty.mkTyCon3 p m n -- package, module, name
