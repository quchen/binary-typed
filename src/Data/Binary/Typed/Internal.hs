{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}



-- | Internals, exposed mostly for potential use by testsuites and benchmarks.
--
-- __Not recommended to be used from within other independent libraries.__
module Data.Binary.Typed.Internal (

      -- * 'Typed'
        Typed(..)
      , TypeInformation(..)
      , Hash5(..)
      , mkHash5
      , Hash32(..)
      , Hash64(..)
      , typed
      , makeTypeInformation
      , TypeFormat(..)
      , getFormat
      , typecheck
      , erase
      , preserialize

      -- * 'TypeRep'
      , TypeRep(..)
      , stripTypeRep
      , unStripTypeRep
      , hashType5
      , hashed5Split
      , hashType32
      , hashType64

      -- * 'TyCon'
      , TyCon(..)
      , stripTyCon
      , unStripTyCon

) where



import           GHC.Generics
import           Text.Printf
import           Data.Bits ((.&.), (.|.))
import           Control.Applicative
-- import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL

import           Data.Typeable (Typeable, typeOf)
import qualified Data.Typeable as Ty

import           Data.Binary

-- Crypto stuff for hashing
import qualified Data.Digest.Murmur32 as H32
import qualified Data.Digest.Murmur64 as H64



-- ^ Type information stored alongside a value to be serialized, so that the
-- recipient can do consistency checks. See 'TypeFormat' for more detailed
-- information on the fields.
data TypeInformation = Untyped'
                     | Hashed5'  Hash5
                     | Hashed32' Hash32
                     | Hashed64' Hash64
                     | Shown'    Hash32 String
                     | Full'     TypeRep
                     | Cached'   BSL.ByteString
                     deriving (Eq, Ord, Show, Generic)

instance Binary TypeInformation where
      put Untyped'             = putWord8 0
      put (Hashed5' (Hash5 x)) = putWord8 (x .|. 1) -- See 'Hash5' for info
      put (Hashed32' x)        = putWord8 2 >> put x
      put (Hashed64' x)        = putWord8 3 >> put x
      put (Shown'    x y)      = putWord8 4 >> put x >> put y
      put (Full'     x)        = putWord8 5 >> put x
      put (Cached'   x)        = putWord8 6 >> put x

      get = getWord8 >>= \case
            0 -> return Untyped'
            -- "1" case handled at the end
            2 -> fmap   Hashed32' get
            3 -> fmap   Hashed64' get
            4 -> liftA2 Shown'    get get
            5 -> fmap   Full'     get
            6 -> fmap   Cached'   get
            n -> case hashed5Split n of
                  (1, hash) -> return (Hashed5' hash)
                  _ -> fail ("Invalid TypeInformation (tag: " ++ show (hashed5Split n) ++ ")")



-- | Split a 'Word8' into the last 3 bit (used to tag the constructor) and
-- the first 5 (data payload). Used by the 'Binary' instance of
-- 'TypeInformation'.
hashed5Split :: Word8 -> (Word8, Hash5)
hashed5Split x = let hash = mkHash5 x
                     tag  = getHashed5Tag x
                 in  (tag, hash)



getHashed5Tag :: Word8 -> Word8
getHashed5Tag = (.&. 0x7) -- = 00000111



-- | Extract which 'TypeFormat' was used to create a certain 'TypeInformation'.
--
-- If the type is 'Cached'', then the contained information is assumed
-- well-formed. In the public API, this is safe to do, since only well-typed
-- 'Typed' values can be created in the first place.
getFormat :: TypeInformation -> TypeFormat
getFormat (Untyped'  {}) = Untyped
getFormat (Hashed5'  {}) = Hashed5
getFormat (Hashed32' {}) = Hashed32
getFormat (Hashed64' {}) = Hashed64
getFormat (Shown'    {}) = Shown
getFormat (Full'     {}) = Full
getFormat (Cached'   bs) = getFormat (decode bs)



-- | A hash value of a 'TypeRep'. Currently a 5-bit value created using
-- the MurmurHash2 algorithm.
--
-- Since 'TypeInformation' needs 3 bit to store the sort of the
-- 'TypeInformation', the remaining 5 bit per 'Word8' can be used to store a
-- hash value at no additional space cost. For this reason, it is important that
-- the three rightmost bits of any 'Hashed5' are set to zero.
--
-- This type intentionally doesn't have a 'Binary' instance, since its
-- serialization is part of the 'TypeInformation' 'Binary' class exclusively.
newtype Hash5 = Hash5 Word8
      deriving (Eq, Ord, Show)

-- | Smart constructor for 'Hash5' values. Makes sure the rightmost three bits
-- are not set by applying a bit mask to the input.
mkHash5 :: Integral a => a -> Hash5
mkHash5 x = Hash5 (fromIntegral x .&. 0xF8)
                                    -- = 11111000



-- | A hash value of a 'TypeRep'. Currently a 32-bit value created using
-- the MurmurHash2 algorithm.
newtype Hash32 = Hash32 Word32
      deriving (Eq, Ord, Show, Generic)
instance Binary Hash32



-- | A hash value of a 'TypeRep'. Currently a 64-bit value created using
-- the MurmurHash2 algorithm.
newtype Hash64 = Hash64 Word64
      deriving (Eq, Ord, Show, Generic)
instance Binary Hash64



-- | A value suitable to be typechecked using the contained extra type
-- information.
data Typed a = Typed TypeInformation a
      -- ^ Using this data constructor directly is unsafe, as it allows
      -- construction of ill-typed 'Typed' data. Use the 'typed' smart
      -- constructor unless you really need 'Typed'.

-- | \"typed \<format\> \<value\>\"
instance Show a => Show (Typed a) where
      show (Typed ty x) = printf "typed %s (%s)"
                                 (show (getFormat ty))
                                 (show x)

-- | Ensures data is decoded as the appropriate type with high or total
-- confidence (depending on with what 'TypeFormat' the 'Typed' was
-- constructed).
instance (Binary a, Typeable a) => Binary (Typed a) where
      get = do (ty, value) <- get
               either fail return (typecheck (Typed ty value))
               -- NB: 'fail' is safe in Get Monad
      put (Typed ty value) = put (ty, value)



-- | Sometimes it can be beneficial to serialize the type information in
-- advance, so that the maybe costly serialization step does not have to be
-- repeated on every invocation of 'encode'. Preserialization comes at a price
-- though, as the directly contained 'BSL.ByteString' requires its length to
-- be included in the final serialization, yielding a 8-byte overhead for the
-- required 'Data.Int.Int64', and one for the tag of what was serialized
-- (\"shown or full?\").
--
-- This function calculates the serialized version of 'TypeInformation' in
-- cases where the required 9 bytes are negligible (determined by an
-- arbitrary threshold, currently 10*9 bytes).
--
-- Used to make 'Data.Binary.Typed.encodeTyped' more efficient; the source
-- there also makes a good usage example.
preserialize :: TypeInformation -> TypeInformation
preserialize x@(Cached'   {}) = x
preserialize x@(Untyped'  {}) = x
preserialize x@(Hashed5'  {}) = x
preserialize x@(Hashed32' {}) = x
preserialize x@(Hashed64' {}) = x
-- Explicit cases for Shown' and Full' so exhaustiveness can be checked when
-- new constructors are added. (The default pattern of just "x" would do right
-- now as well, but not provide that.)
preserialize x@(Shown'    {}) = preserialize' x
preserialize x@(Full'     {}) = preserialize' x



-- | Preserializes type information if its encoded byte length is larger than
-- an arbitrary threshold. Less efficient than 'preserialize' since it
-- always preserializes and always calculates the encoded version no matter
-- what.
preserialize' :: TypeInformation -> TypeInformation
preserialize' x | BSL.length encoded > 10*9 = Cached' encoded
                | otherwise = x
                where encoded = encode x



-- | Different ways of including/verifying type information of serialized
--   messages.
data TypeFormat =

        -- | Include no type information.
        --
        --   * Requires one byte more compared to using 'Binary' directly
        --     (to tag the data as untyped, required for the decoding step).
        --
        --   * Encoding and decoding require negligible amount of additional
        --     computational cost compared to direct (intrinsically untyped)
        --     'Binary'.
        Untyped

        -- | Like 'Hashed32', but uses a 5-bit hash value.
        --
        -- * Requires the same amount of space as 'Untyped', i.e. the only
        --   overhead compared to it is the computational cost to calculate
        --   the hash, which is almost identical to the one of 'Hashed32'.
        --
        -- * Collisions occur with a probability of 1/2^5 = 1/32. For this
        --   reason, this format is only recommended when minimal data size
        --   is top priority.
        --
      | Hashed5

        -- | Compare types by their hash values (using the MurmurHash2
        -- algorithm).
        --
        -- * Requires five bytes more compared to using 'Binary' directly for
        --   the type information (one to tag as 'Hashed32', four for the
        --   hash value)
        --
        -- * Subject to false positive due to hash collisions, although in
        --   practice this should almost never happen.
        --
        -- * Type errors cannot tell the provided type ("Expected X, received
        --   type with hash H")
        --
        -- * Computational cost similar to 'Hashed64'.
      | Hashed32

        -- | Like 'Hashed32', but uses a 64-bit hash value.
        --
        -- * Requires nine bytes more compared to using 'Binary'.
        --
        -- * Hash collisions are even less likely to occur than with
        --   'Hashed32'.
        --
        -- * Computational cost similar to 'Hashed32'.
      | Hashed64

        -- | Compare 'String' representation of types, obtained by calling
        -- 'show' on the 'TypeRep', and also include a hash value
        -- (like 'Hashed32'). The former is mostly for readable error
        -- messages, the latter provides better collision resistance.
        --
        -- * Data size larger than 'Hashed32', but usually smaller than
        --   'Full'.
        --
        -- * Both the hash and the shown type must match to satisfy the
        --   typechecker.
        --
        -- * Useful type errors ("expected X, received Y"). All types are
        --   shown unqualified though, making @Foo.X@ and @Bar.X@ look
        --   identical in error messages. Remember this when you get a
        --   seemingly silly error "expected Foo, but given Foo".
      | Shown

        -- | Compare the full representation of a data type.
        --
        -- * More verbose than 'Hashed' and 'Shown'. As a rule of thumb,
        --   transmitted data is roughly the same as 'Shown', but all names
        --   are fully qualified (package, module, type name).
        -- * Correct comparison (no false positives). An semi-exception here
        --   is when types change between package versions:
        --   @package-1.0 Foo.X@ and @package-1.1 Foo.X@ count as the same
        --   type.
        -- * Useful type errors ("expected X, received Y"). All types are
        --   shown unqualified though, making @Foo.X@ and @Bar.X@ look
        --   identical in error messages. Remember this when you get a
        --   seemingly silly error "expected Foo, but given Foo".
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
typed format x = Typed (makeTypeInformation format (typeOf x)) x



-- | Create the 'TypeInformation' to be stored inside a 'Typed' value from
--   a 'Ty.TypeRep'.
makeTypeInformation :: TypeFormat -> Ty.TypeRep -> TypeInformation
makeTypeInformation format ty = case format of
      Untyped  -> Untyped'
      Hashed5  -> Hashed5'   (hashType5    ty)
      Hashed32 -> Hashed32'  (hashType32   ty)
      Hashed64 -> Hashed64'  (hashType64   ty)
      Shown    -> Shown'     (hashType32   ty) (show ty)
      Full     -> Full'      (stripTypeRep ty)




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
      Full'     full     | exFull /= full     -> Left (fullError full)
      Hashed5'  hash5    | exHash5 /= hash5   -> Left (hashError exHash5  hash5)
      Hashed32' hash32   | exHash32 /= hash32 -> Left (hashError exHash32 hash32)
      Hashed64' hash64   | exHash64 /= hash64 -> Left (hashError exHash64 hash64)
      Shown'  hash32 str | (exHash32, exShow) /= (hash32, str)
                                              -> Left (shownError hash32 str)
      _no_type_error -> Right ty


      where

      -- ex = expected
      exType   = typeOf x
      exHash5  = hashType5    exType
      exHash32 = hashType32   exType
      exHash64 = hashType64   exType
      exShow   = show         exType
      exFull   = stripTypeRep exType

      hashError eHash hash = printf pat exShow (show eHash) (show hash)
            where pat = "Type error: expected type %s with hash %s,\
                        \ but received data with hash %s"
      shownError hash str = printf pat exShow (show exHash32) str (show hash)
            where pat = "Type error: expected type %s and hash %s,\
                        \ but received data with type %s and hash %s"
      fullError full = printf pat exShow (show full)
            where pat = "Type error: expected type %s,\
                        \ but received data with type %s"

      decode' bs = case decodeOrFail bs of
            Left  (_,_,err) -> Left  ("Cache error! " ++ err)
            Right (_,_,val) -> Right val



-- | Hash a 'Ty.TypeRep' to a 5-bit digest.
hashType5 :: Ty.TypeRep -> Hash5
hashType5 = mkHash5 . H32.asWord32 . H32.hash32 . stripTypeRep



-- | Hash a 'Ty.TypeRep' to a 32-bit digest.
hashType32 :: Ty.TypeRep -> Hash32
hashType32 = Hash32 . H32.asWord32 . H32.hash32 . stripTypeRep



-- | Hash a 'Ty.TypeRep' to a 64-bit digest.
hashType64 :: Ty.TypeRep -> Hash64
hashType64 = Hash64 . H64.asWord64 . H64.hash64 . stripTypeRep



-- | 'Ty.TypeRep' without the (internal) fingerprint.
data TypeRep = TypeRep TyCon [TypeRep]
      deriving (Eq, Ord, Generic)
instance Binary TypeRep

instance Show TypeRep where
      show = show . unStripTypeRep

instance H32.Hashable32 TypeRep where
      hash32Add (TypeRep tycon args) = H32.hash32Add (tycon, args)

instance H64.Hashable64 TypeRep where
      hash64Add (TypeRep tycon args) = H64.hash64Add (tycon, args)




-- | 'Ty.TyCon' without the (internal) fingerprint.
data TyCon = TyCon String String String -- ^ Package, module, constructor name
      deriving (Eq, Ord, Generic)
instance Binary TyCon

instance Show TyCon where
      show = show . unStripTyCon

instance H32.Hashable32 TyCon where
      hash32Add (TyCon p m c) = H32.hash32Add (p, m, c)

instance H64.Hashable64 TyCon where
      hash64Add (TyCon p m c) = H64.hash64Add (p, m, c)



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
