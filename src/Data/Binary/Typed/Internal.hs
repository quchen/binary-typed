{-# LANGUAGE DeriveGeneric #-}



-- | Internals, exposed mostly for potential use by testsuites and benchmarks.
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
      , precache

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
import           Text.Printf
-- import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL

import           Data.Typeable (Typeable, typeOf)
import qualified Data.Typeable as Ty

import           Data.Binary

-- Crypto stuff for hashing
import           Data.Digest.Murmur64



-- ^ Type information stored alongside a value to be serialized, so that the
--   recipient can do consistency checks. See 'TypeFormat' for more detailed
--   information on the fields.
data TypeInformation = Untyped'
                     | Hashed'  Hash
                     | Shown'   Hash String
                     | Full'    TypeRep
                     | Cached'  BSL.ByteString
                     deriving (Eq, Ord, Show, Generic)

instance Binary TypeInformation



-- | Extract which 'TypeFormat' was used to create a certain 'TypeInformation'.
getFormat :: TypeInformation -> TypeFormat
getFormat (Untyped' {}) = Untyped
getFormat (Hashed'  {}) = Hashed
getFormat (Shown'   {}) = Shown
getFormat (Full'    {}) = Full
getFormat (Cached'  bs) = getFormat (decode bs)
                        -- decode is safe here since caching ensures
                        -- a well-formed input ByteString



-- | A hash value of a 'TypeRep'. Currently a 64-bit value created using
--   the MurmurHash2 algorithm.
newtype Hash = Hash Word64
      deriving (Eq, Ord, Show, Generic)
instance Binary Hash



-- | A value suitable to be typechecked using the contained extra type
--   information.
data Typed a = Typed TypeInformation a
      -- ^ Using this data constructor directly is unsafe, as it allows
      -- construction of ill-typed 'Typed' data. Use the 'typed' smart
      -- constructor unless you really need 'Typed'.

-- | "typed \<format\> \<value\>"
instance Show a => Show (Typed a) where
      show (Typed ty x) = printf "typed %s (%s)"
                                 (show (getFormat ty))
                                 (show x)

-- | Ensures data is decoded as the appropriate type with high or total
--   confidence (depending on with what 'TypeFormat' the 'Typed' was
--   constructed).
instance (Binary a, Typeable a) => Binary (Typed a) where
      get = do (ty, value) <- get
               either fail return (typecheck (Typed ty value))
               -- NB: 'fail' is safe in Get Monad
      put (Typed ty value) = put (ty, value)



-- | Calculate the serialization of a 'TypeInformation' and store it in a
--   'Typed' value so it does not have to be recalculated on every call to
--   'encode'.
--
--   This is typically applied to a dummy value created using 'typed' and
--   the desired 'TypeFormat'; the actual data is then inserted using
--   'Data.Binary.Typed.reValue', which is how
--   'Data.Binary.Typed.encodeTyped' works.
precache :: Typed a -> Typed a
precache t@(Typed (Cached' _) _) = t
precache   (Typed ty          x) = Typed (Cached' (encode ty)) x
                                   -- This is the only place that constructs a
                                   -- Cached' value.



-- | Different ways of including/verifying type information of serialized
--   messages.
data TypeFormat =

        -- | Include no type information.
        --
        --   * Requires one byte more compared to using 'Binary' directly
        --     (to tag the data as untyped, required for the decoding step).
        Untyped

        -- | Compare types by their hash values (using the MurmurHash2
        --   algorithm).
        --
        --   * Requires nine bytes more compared to using 'Binary' directly for
        --     the type information (one to tag as 'Hashed', eight for the hash
        --     value)
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
        --     shown unqualified though, making @Foo.X@ and @Bar.X@ look
        --     identical in error messages.
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
        --     shown unqualified though, making @Foo.X@ and @Bar.X@ look
        --     identical in error messages.
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
            Untyped -> Untyped'
            Hashed  -> Hashed'  (hashType     ty)
            Shown   -> Shown'   (hashType     ty) (show ty)
            Full    -> Full'    (stripTypeRep ty)



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



-- | Typecheck a 'Typed'. Returns the (well-typed) input, or an error message
--   if the types don't work out.
typecheck :: Typeable a => Typed a -> Either String (Typed a)
typecheck ty@(Typed typeInformation x) = case typeInformation of
      Cached' cache -> decode' cache >>= \ty' -> typecheck (Typed ty' x)
      Full'   full     | exFull /= full -> Left (fullError full)
      Hashed' hash     | exHash /= hash -> Left (hashError hash)
      Shown'  hash str | (exHash, exShow) /= (hash, str)
                                        -> Left (shownError hash str)
      _no_type_error -> Right ty


      where

      -- ex = expected
      exType = typeOf x
      exHash = hashType     exType
      exShow = show         exType
      exFull = stripTypeRep exType

      hashError hash = printf pat exShow (show exHash) (show hash)
            where pat = "Type error: expected type %s with hash %s,\
                        \ but received data with hash %s"
      shownError hash str = printf pat exShow (show exHash) str (show hash)
            where pat = "Type error: expected type %s and hash %s,\
                        \ but received data with type %s and hash %s"
      fullError full = printf pat exShow (show full)
            where pat = "Type error: expected type %s,\
                        \ but received data with type %s"

      decode' bs = case decodeOrFail bs of
            Left  (_,_,err) -> Left  ("Cache error! " ++ err)
            Right (_,_,val) -> Right val



-- | Hash a 'Ty.TypeRep'.
hashType :: Ty.TypeRep -> Hash
hashType = Hash . asWord64 . hash64 . stripTypeRep



-- | 'Ty.TypeRep' without the (internal) fingerprint.
data TypeRep = TypeRep TyCon [TypeRep]
      deriving (Eq, Ord, Generic)
instance Binary TypeRep

instance Show TypeRep where
      show = show . unStripTypeRep

instance Hashable64 TypeRep where
      hash64Add (TypeRep tycon args) = hash64Add (tycon, args)




-- | 'Ty.TyCon' without the (internal) fingerprint.
data TyCon = TyCon String String String -- ^ Package, module, constructor name
      deriving (Eq, Ord, Generic)
instance Binary TyCon

instance Show TyCon where
      show = show . unStripTyCon

instance Hashable64 TyCon where
      hash64Add (TyCon p m c) = hash64Add (p, m, c)



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
