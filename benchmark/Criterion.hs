{-# LANGUAGE ExistentialQuantification #-}

import Criterion.Main


import Data.Binary
import Data.Binary.Typed
import Data.Typeable
import Control.DeepSeq
import Control.Exception (evaluate)






-- Test values

someInt :: Int
someInt = 12345

someShortString :: String
someShortString = "Hello"

someLongString :: String
someLongString = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam\
                 \ vitae lacinia tellus. Maecenas posuere."

someComplicated :: Complicated
someComplicated = Right (Left "Hello")


-- | Data with a normal form.
data NF = forall a. NFData a => NF a

-- | Evaluate 'NF' data to normal form.
force' :: NF -> ()
force' (NF x) = x `deepseq` ()



type Complicated = Either (Char, Int) (Either String (Maybe Integer))



main :: IO ()
main = do
      forceCafs
      defaultMain [ bgroup "Encode" bench_binaryVsTyped ]

-- | List of all data that should be fully evaluated before the benchmark is
--   run.
cafs :: [NF]
cafs =
      [ NF someInt
      , NF someShortString
      , NF someLongString
      ]

forceCafs :: IO ()
forceCafs = mapM_ (evaluate . force') cafs


bench_binaryVsTyped :: [Benchmark]
bench_binaryVsTyped =
      [ bgroup "Int"
            [ bench_int_untyped
            , bgroup "recalculate" bench_int
            ]
      , bgroup "\"hello\""
            [ bench_short_string_untyped
            , bgroup "recalculate" bench_short_string
            ]
      , bgroup "Lipsum (length 100)"
            [ bench_long_string_untyped
            , bgroup "recalculate" bench_long_string
            ]
      , bgroup "Complicated type"
            [ bench_complicated_untyped
            , bgroup "recalculate" bench_complicated
            ]
      ]



bench_int :: [Benchmark]
bench_int = map (bench_encode someInt) formats

bench_int_untyped :: Benchmark
bench_int_untyped = bench "Binary only" (nf encode someInt)

bench_short_string :: [Benchmark]
bench_short_string = map (bench_encode someShortString) formats

bench_short_string_untyped :: Benchmark
bench_short_string_untyped = bench "Binary only" (nf encode someShortString)

bench_long_string :: [Benchmark]
bench_long_string = map (bench_encode someLongString) formats

bench_long_string_untyped :: Benchmark
bench_long_string_untyped = bench "Binary only" (nf encode someLongString)

bench_complicated :: [Benchmark]
bench_complicated = map (bench_encode someComplicated) formats

bench_complicated_untyped :: Benchmark
bench_complicated_untyped = bench "Binary only" (nf encode someComplicated)

formats :: [TypeFormat]
formats = [ Untyped
          , Hashed32
          , Hashed64
          , Shown
          , Full
          ]

-- | Simply encode a value using the specified 'TypeFormat'.
bench_encode
      :: (Binary a, Typeable a)
      => a
      -> TypeFormat
      -> Benchmark
bench_encode x format = bench description (nf (encodeTyped format) x)
      where description = "Typed: " ++ show format
