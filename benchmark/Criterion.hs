import Criterion.Main


import Data.Binary
import Data.Binary.Typed
import Data.Typeable






-- Test values

someInt :: Int
someInt = 12345

someShortString :: String
someShortString = "Hello"

someLongString :: String
someLongString = take 1000 ['a'..]









main :: IO ()
main = defaultMain
      [ bgroup "Encode" bench_binaryVsTyped
      ]

bench_binaryVsTyped :: [Benchmark]
bench_binaryVsTyped =
      [ bgroup "Int"               [ bgroup "recalculate" bench_int
                                   , bgroup "precache"    (bench_encode_precached
                                                                   intValUntyped
                                                                   intValHashed
                                                                   intValShown
                                                                   intValFull
                                                                   someInt)
                                   ]
      , bgroup "\"hello\""         [ bgroup "recalculate" bench_short_string
                                   , bgroup "precache"    (bench_encode_precached
                                                                   strSValUntyped
                                                                   strSValHashed
                                                                   strSValShown
                                                                   strSValFull
                                                                   someShortString)
                                   ]
      , bgroup "take 1000 ['a'..]" [ bgroup "recalculate" bench_long_string
                                   , bgroup "precache"    (bench_encode_precached
                                                                   strLValUntyped
                                                                   strLValHashed
                                                                   strLValShown
                                                                   strLValFull
                                                                   someLongString)
                                   ]
      ]

      where

      defaultInt :: Int
      defaultInt = 0

      defaultString :: String
      defaultString = ""

      intValUntyped = typed  Untyped defaultInt
      intValHashed  = typed  Hashed  defaultInt
      intValShown   = typed  Shown   defaultInt
      intValFull    = typed  Full    defaultInt

      strSValUntyped = typed Untyped defaultString
      strSValHashed  = typed Hashed  defaultString
      strSValShown   = typed Shown   defaultString
      strSValFull    = typed Full    defaultString

      strLValUntyped = typed Untyped defaultString
      strLValHashed  = typed Hashed  defaultString
      strLValShown   = typed Shown   defaultString
      strLValFull    = typed Full    defaultString



bench_int :: [Benchmark]
bench_int = map (bench_encode someInt) formats

bench_short_string :: [Benchmark]
bench_short_string = map (bench_encode someShortString) formats

bench_long_string :: [Benchmark]
bench_long_string = map (bench_encode someLongString) formats

formats :: [Maybe TypeFormat]
formats = [ Nothing
          , Just Untyped
          , Just Hashed
          , Just Shown
          , Just Full
          ]

-- | Encdes using 'Binary' if the second argument is 'Nothing', otherwise using
--   'Typed' with the supplied 'TypeFormat'.
bench_encode
      :: (Binary a, Typeable a)
      => a
      -> Maybe TypeFormat
      -> Benchmark
bench_encode x Nothing       = bench "Binary only" (nf encode               x)
bench_encode x (Just format) = bench description   (nf (encodeTyped format) x)
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