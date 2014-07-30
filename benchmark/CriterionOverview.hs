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
mode = Hashed64

type Complicated = Either (Char, Int) (Either String (Maybe Integer))

value :: Complicated
value = Right (Left ("Lorem ipsum dolor sit amet, consectetur adipiscing elit.\
                     \ Nam vitae lacinia tellus. Maecenas posuere."))

-- | Pre-define encoding function so sharing can work properly
encodeTypedEncoder :: Complicated -> ByteString
encodeTypedEncoder = encodeTyped mode

decodeTypedUnsafely :: ByteString -> Complicated
decodeTypedUnsafely = unsafeDecodeTyped'

-- Precalcualte encoded values for decoding benchmark
value_encodedBinary, value_encodedTyped :: ByteString
value_encodedBinary = encode value
value_encodedTyped  = encodeTyped mode value



main :: IO ()
main = do
      evaluate (value               `deepseq` ())
      evaluate (value_encodedBinary `deepseq` ())
      evaluate (value_encodedTyped  `deepseq` ())
      evaluate (encodeTypedEncoder      `seq` ())

      defaultMain [ bgroup "encode"        bench_encode
                  , bgroup "decode"        bench_decode
                  , bgroup "encode+decode" bench_both
                  ]


-- #############################################################################
-- ###  Encode only  ###########################################################
-- #############################################################################



bench_encode :: [Benchmark]
bench_encode = [ bench_encode_binaryOnly
               , bench_encode_typed
               ]

bench_encode_binaryOnly :: Benchmark
bench_encode_binaryOnly = bench d (nf f value)
      where d = "Binary"
            f = encode

bench_encode_typed :: Benchmark
bench_encode_typed = bench d (nf f value)
      where d = "Typed with " ++ show mode
            f = encodeTypedEncoder



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
      where d = "Typed with " ++ show mode
            f :: ByteString -> Complicated
            f = decodeTypedUnsafely





-- #############################################################################
-- ###  Encode+decode  #########################################################
-- #############################################################################

bench_both :: [Benchmark]
bench_both = [ bench_both_binaryOnly
             , bench_both_typed
             ]


bench_both_binaryOnly :: Benchmark
bench_both_binaryOnly = bench d (nf f value)
      where d = "Binary only"
            f :: Complicated -> Complicated
            f = decode . encode

bench_both_typed :: Benchmark
bench_both_typed = bench d (nf f value)
      where d = "Typed with " ++ show mode
            f :: Complicated -> Complicated
            f = decodeTypedUnsafely . encodeTypedEncoder
