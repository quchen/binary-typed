-- TODO: Support different equalities

import Data.Word
import Data.Typeable
import Data.Typeable.Internal as TI
import Data.Binary
import qualified Data.Binary.Get as Get
import Control.Applicative
import qualified Data.ByteString.Lazy as BSL

import Test.QuickCheck



newtype Typed a = Typed a
      deriving (Show, Eq)

typed :: Typeable a => a -> Typed a
typed = Typed

erase :: Typed a -> a
erase (Typed x) = x


instance Binary TI.Fingerprint where
      get = liftA2 TI.Fingerprint get get
      put (TI.Fingerprint a b) = put a *> put b

instance Binary TI.TyCon where
      get = TI.TyCon <$> get <*> get <*> get <*> get
      put (TI.TyCon fp b c d) = put fp *> put b *> put c *> put d

instance Binary TI.TypeRep where
      get = TI.TypeRep <$> get <*> get <*> get
      put (TI.TypeRep fp b c) = put fp *> put b *> put c

instance (Binary a, Typeable a) => Binary (Typed a) where
      get = do (shouldType, value) <- get
               let isType = typeOf value
               if shouldType == isType
                     then return (Typed value)
                     else fail ("TYPE ERROR: expected " ++ show shouldType ++ ", got " ++ show isType)

      put (Typed x) = put (typeOf x, x)

main2 = do
      let val = 0 :: Word8
      print val
      putStrLn "==================="
      let enc = encode (Typed val)
      print enc
      putStrLn "==================="
      let dec = decodeOrFail enc :: Either (BSL.ByteString, Get.ByteOffset, String) (BSL.ByteString, Get.ByteOffset, Typed Word8)
      print dec

foo = do
      let val = 0 :: Word8
      print val
      let enc = encode (Typed val)
      print enc
      let dec = decodeOrFail enc :: Either (BSL.ByteString, Get.ByteOffset, String) (BSL.ByteString, Get.ByteOffset, Word8)
      print dec




-- #############################################################################
-- #############################################################################
-- #############################################################################

instance Arbitrary TI.Fingerprint where
      arbitrary = liftA2 TI.Fingerprint arbitrary arbitrary

instance Arbitrary TI.TyCon where
      arbitrary = TI.TyCon <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TI.TypeRep where
      arbitrary = TI.TypeRep <$> arbitrary <*> arbitrary <*> resize 1 arbitrary

instance (Arbitrary a) => Arbitrary (Typed a) where
      arbitrary = fmap Typed arbitrary


prop_fingerprint :: TI.Fingerprint -> Bool
prop_fingerprint fp = fp == decode (encode fp)
prop_tycon :: TI.TyCon -> Bool
prop_tycon tc = tc == decode (encode tc)
prop_typerep :: TI.TypeRep -> Bool
prop_typerep tr = tr == decode (encode tr)
prop_typed :: Typed Int -> Bool
prop_typed ty = ty == decode (encode ty)


qc = do
      quickCheck prop_fingerprint
      quickCheck prop_tycon
      quickCheck prop_typerep
      quickCheck prop_typed