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
someLongString = take 1000 ['a'..]


-- | Data with a normal form.
data NF = forall a. NFData a => NF a

-- | Evaluate 'NF' data to normal form.
force' :: NF -> ()
force' (NF x) = x `deepseq` ()






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
      , NF (encode intValHashed)
      , NF (encode intValShown)
      , NF (encode intValFull)
      , NF (encode strSValUntyped)
      , NF (encode strSValHashed)
      , NF (encode strSValShown)
      , NF (encode strSValFull)
      , NF (encode strLValUntyped)
      , NF (encode strLValHashed)
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
                                                           intValHashed
                                                           intValShown
                                                           intValFull
                                                           someInt)
            ]
      , bgroup "\"hello\""
            [ bench_short_string_untyped
            , bgroup "recalculate" bench_short_string
            , bgroup "precache"    (bench_encode_precached strSValUntyped
                                                           strSValHashed
                                                           strSValShown
                                                           strSValFull
                                                           someShortString)
            ]
      , bgroup "take 1000 ['a'..]"
            [ bench_long_string_untyped
            , bgroup "recalculate" bench_long_string
            , bgroup "precache"    (bench_encode_precached strLValUntyped
                                                           strLValHashed
                                                           strLValShown
                                                           strLValFull
                                                           someLongString)
            ]
      ]



defaultInt :: Int
defaultInt = 0

defaultString :: String
defaultString = ""

intValUntyped, intValHashed, intValShown, intValFull :: Typed Int
intValUntyped = typed  Untyped defaultInt
intValHashed  = typed  Hashed  defaultInt
intValShown   = typed  Shown   defaultInt
intValFull    = typed  Full    defaultInt

strSValUntyped, strSValHashed, strSValShown, strSValFull :: Typed String
strSValUntyped = typed Untyped defaultString
strSValHashed  = typed Hashed  defaultString
strSValShown   = typed Shown   defaultString
strSValFull    = typed Full    defaultString

strLValUntyped, strLValHashed, strLValShown, strLValFull :: Typed String
strLValUntyped = typed Untyped defaultString
strLValHashed  = typed Hashed  defaultString
strLValShown   = typed Shown   defaultString
strLValFull    = typed Full    defaultString



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

formats :: [TypeFormat]
formats = [ Untyped
          , Hashed
          , Shown
          , Full
          ]

-- | Encdes using 'Binary' if the second argument is 'Nothing', otherwise using
--   'Typed' with the supplied 'TypeFormat'.
bench_encode
      :: (Binary a, Typeable a)
      => a
      -> TypeFormat
      -> Benchmark
bench_encode x format = bench description (nf (encodeTyped format) x)
      where description = "Typed: " ++ show format



bench_encode_precached
      :: (Binary a, Typeable a)
      => Typed a -- ^ Precached 'Untyped' dummy value
      -> Typed a -- ^ dito, with 'Hashed'
      -> Typed a -- ^ dito, with 'Shown'
      -> Typed a -- ^ dito, with 'Full'
      -> a       -- ^ Actual value to encode
      -> [Benchmark]
bench_encode_precached untyped hashed shown full x =
      [ bench (description Untyped) (nf (encode . reValue (const x)) untyped)
      , bench (description Hashed)  (nf (encode . reValue (const x)) hashed)
      , bench (description Shown)   (nf (encode . reValue (const x)) shown)
      , bench (description Full)    (nf (encode . reValue (const x)) full)
      ]
      where description format = "Typed: " ++ show format