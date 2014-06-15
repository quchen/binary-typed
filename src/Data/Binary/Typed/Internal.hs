{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}



-- | Internals, exposed mostly for potential use by the testsuites.
--
-- __Not recommended to be used from within other independent libraries.__
module Data.Binary.Typed.Internal (
      -- * Core types
        Typed(..)
      , TypeInformation(..)

      -- * Construction
      , typed
      , TypeFormat(..)

      -- * Verification
      , typecheck

      -- * Deconstruction
      , erase

      -- * Type information generators
      , typeHash
      , getFull
) where


import           GHC.Generics
import           Numeric (showHex)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL

import           Data.Typeable (Typeable, typeOf)
import qualified Data.Typeable as Ty

import           Data.Binary

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
                                        , showBSHex expectedHash ++ ","
                                        , "but received data with hash"
                                        , showBSHex hash
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

-- | Show a 'BS.ByteString' in lower-case hex format (e.g. @1234abc567@).
showBSHex :: BS.ByteString -> String
showBSHex = concatMap (\x -> showHex x "") . BS.unpack



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
