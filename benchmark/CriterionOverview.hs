-- | This generates the benchmark to be shown in the readme and the .cabal
--   file.

{-# LANGUAGE ExistentialQuantification #-}

import Criterion.Main


import Data.Binary
import Data.Binary.Typed
import Data.ByteString.Lazy (ByteString)
import Control.DeepSeq
import Control.Exception (evaluate)


-- Encoding mode to be used
mode :: TypeFormat
mode = Hashed

type Complicated = Either (Char, Int) (Either String (Maybe Integer))

value :: Complicated
value = Right (Left ("Lorem ipsum dolor sit amet, consectetur adipiscing elit.\
                     \ Nam vitae lacinia tellus. Maecenas posuere."))

-- Precalcualte encoded values for decoding benchmark
value_encodedBinary, value_encodedTyped :: ByteString
value_encodedBinary = encode value
value_encodedTyped  = encodeTyped mode value



-- Cached typed encoder
encodeTyped' :: Complicated -> ByteString
encodeTyped' = encodeTypedLike (typed mode dummyValue)
      where dummyValue :: Complicated
            dummyValue = Left ('x', 0)



main :: IO ()
main = do
      evaluate (value `deepseq` ())
      defaultMain [ bgroup "encode only"   bench_encode
                  , bgroup "decode only"   bench_decode
                  , bgroup "encode+decode" bench_both
                  ]


-- #############################################################################
-- ###  Encode only  ###########################################################
-- #############################################################################



bench_encode :: [Benchmark]
bench_encode = [ bench_encode_binaryOnly
               , bench_encode_uncached
               , bench_encode_cached
               ]

bench_encode_binaryOnly :: Benchmark
bench_encode_binaryOnly = bench d (nf f value)
      where d = "Binary only"
            f = encode

bench_encode_uncached :: Benchmark
bench_encode_uncached = bench d (nf f value)
      where d = "Uncached"
            f = encodeTyped mode

bench_encode_cached :: Benchmark
bench_encode_cached = bench d (nf f value)
      where d = "Cached"
            f = encodeTyped'





-- #############################################################################
-- ###  Decode only  ###########################################################
-- #############################################################################



bench_decode :: [Benchmark]
bench_decode = [ bench_decode_binaryOnly
               , bench_decode_typed
               ]

bench_decode_binaryOnly :: Benchmark
bench_decode_binaryOnly = bench d (nf f value_encodedBinary)
      where d = "Binary only"
            f :: ByteString -> Complicated
            f = decode

bench_decode_typed :: Benchmark
bench_decode_typed = bench d (nf f value_encodedTyped)
      where d = "Typed"
            f :: ByteString -> Complicated
            f = unsafeDecodeTyped





-- #############################################################################
-- ###  Encode+decode  #########################################################
-- #############################################################################

bench_both :: [Benchmark]
bench_both = [ bench_both_binaryOnly
             , bench_both_uncached
             , bench_both_cached
             ]


bench_both_binaryOnly :: Benchmark
bench_both_binaryOnly = bench d (nf f value)
      where d = "Binary only"
            f :: Complicated -> Complicated
            f = decode . encode

bench_both_uncached :: Benchmark
bench_both_uncached = bench d (nf f value)
      where d = "Uncached"
            f :: Complicated -> Complicated
            f = unsafeDecodeTyped . encodeTyped mode

bench_both_cached :: Benchmark
bench_both_cached = bench d (nf f value)
      where d = "Cached"
            f :: Complicated -> Complicated
            f = unsafeDecodeTyped . encodeTyped'
