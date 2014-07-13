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
      , NF defaultInt
      , NF defaultString
      , NF (encode intValUntyped) -- Evaluate the encodings to NF to force a 'Typed'. Hacky but works :-)
      , NF (encode intValHashed32)
      , NF (encode intValHashed64)
      , NF (encode intValShown)
      , NF (encode intValFull)
      , NF (encode strSValUntyped)
      , NF (encode strSValHashed32)
      , NF (encode strSValHashed64)
      , NF (encode strSValShown)
      , NF (encode strSValFull)
      , NF (encode strLValUntyped)
      , NF (encode strLValHashed32)
      , NF (encode strLValHashed64)
      , NF (encode strLValShown)
      , NF (encode strLValFull)
      ]

forceCafs :: IO ()
forceCafs = mapM_ (evaluate . force') cafs


bench_binaryVsTyped :: [Benchmark]
bench_binaryVsTyped =
      [ bgroup "Int"
            [ bench_int_untyped
            , bgroup "recalculate" bench_int
            , bgroup "precache"    (bench_encode_precached intValUntyped
                                                           intValHashed32
                                                           intValHashed64
                                                           intValShown
                                                           intValFull
                                                           someInt)
            ]
      , bgroup "\"hello\""
            [ bench_short_string_untyped
            , bgroup "recalculate" bench_short_string
            , bgroup "precache"    (bench_encode_precached strSValUntyped
                                                           strSValHashed32
                                                           strSValHashed64
                                                           strSValShown
                                                           strSValFull
                                                           someShortString)
            ]
      , bgroup "Lipsum (length 100)"
            [ bench_long_string_untyped
            , bgroup "recalculate" bench_long_string
            , bgroup "precache"    (bench_encode_precached strLValUntyped
                                                           strLValHashed32
                                                           strLValHashed64
                                                           strLValShown
                                                           strLValFull
                                                           someLongString)
            ]
      , bgroup "Complicated type"
            [ bench_complicated_untyped
            , bgroup "recalculate" bench_complicated
            , bgroup "precache"    (bench_encode_precached compLValUntyped
                                                           compLValHashed32
                                                           compLValHashed64
                                                           compLValShown
                                                           compLValFull
                                                           someComplicated)
            ]
      ]



defaultInt :: Int
defaultInt = 0

defaultString :: String
defaultString = ""

defaultComplicated :: Complicated
defaultComplicated = Left (' ', 0)

intValUntyped, intValHashed32, intValHashed64, intValShown, intValFull :: Typed Int
intValUntyped  = precache (typed Untyped  defaultInt)
intValHashed32 = precache (typed Hashed32 defaultInt)
intValHashed64 = precache (typed Hashed64 defaultInt)
intValShown    = precache (typed Shown    defaultInt)
intValFull     = precache (typed Full     defaultInt)

strSValUntyped, strSValHashed32, strSValHashed64, strSValShown, strSValFull :: Typed String
strSValUntyped  = precache (typed Untyped  defaultString)
strSValHashed32 = precache (typed Hashed32 defaultString)
strSValHashed64 = precache (typed Hashed64 defaultString)
strSValShown    = precache (typed Shown    defaultString)
strSValFull     = precache (typed Full     defaultString)

strLValUntyped, strLValHashed32, strLValHashed64, strLValShown, strLValFull :: Typed String
strLValUntyped  = precache (typed Untyped  defaultString)
strLValHashed32 = precache (typed Hashed32 defaultString)
strLValHashed64 = precache (typed Hashed64 defaultString)
strLValShown    = precache (typed Shown    defaultString)
strLValFull     = precache (typed Full     defaultString)

compLValUntyped, compLValHashed32, compLValHashed64, compLValShown, compLValFull :: Typed Complicated
compLValUntyped  = precache (typed Untyped  defaultComplicated)
compLValHashed32 = precache (typed Hashed32 defaultComplicated)
compLValHashed64 = precache (typed Hashed64 defaultComplicated)
compLValShown    = precache (typed Shown    defaultComplicated)
compLValFull     = precache (typed Full     defaultComplicated)



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



-- | Encode a value using a precached 'Typed' value.
bench_encode_precached
      :: (Binary a, Typeable a)
      => Typed a -- ^ Precached 'Untyped' dummy value
      -> Typed a -- ^ dito, with 'Hashed32'
      -> Typed a -- ^ dito, with 'Hashed64'
      -> Typed a -- ^ dito, with 'Shown'
      -> Typed a -- ^ dito, with 'Full'
      -> a       -- ^ Actual value to encode
      -> [Benchmark]
bench_encode_precached untyped hashed32 hashed64 shown full x =
      [ bench (description Untyped)   (nf (encodeTypedLike untyped ) x)
      , bench (description Hashed32)  (nf (encodeTypedLike hashed32) x)
      , bench (description Hashed64)  (nf (encodeTypedLike hashed64) x)
      , bench (description Shown)     (nf (encodeTypedLike shown   ) x)
      , bench (description Full)      (nf (encodeTypedLike full    ) x)
      ]
      where description format = "Typed: " ++ show format