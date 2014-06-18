import Criterion.Main


import Data.Binary
import Data.Binary.Typed
import Data.Typeable

main :: IO ()
main = defaultMain
      [ bgroup "Binary vs. typed binary" bench_binaryVsTyped
      ]

bench_binaryVsTyped :: [Benchmark]
bench_binaryVsTyped =
      [ bgroup "Int"                      (map (bench_encode (12345 :: Int)    ) formats)
      , bgroup "String: \"hello\""        (map (bench_encode  "hello"          ) formats)
      , bgroup "String: take 100 ['a'..]" (map (bench_encode (take 100 ['a'..])) formats)
      ]

formats :: [Maybe TypeFormat]
formats = [ Nothing
          , Just Untyped
          , Just Hashed
          , Just Shown
          , Just Full
          ]

-- | Encdes using 'Binary' if the second argument is 'Nothing', otherwise using
--   'Typed' with the supplied 'TypeFormat'.
bench_encode :: (Binary a, Typeable a) => a -> Maybe TypeFormat -> Benchmark
bench_encode x Nothing       = bench "Binary only" (nf encode               x)
bench_encode x (Just format) = bench description   (nf (encodeTyped format) x)
      where description = "Typed: " ++ show format
