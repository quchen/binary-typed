module Main where

import Data.Binary
import Data.Binary.Typed

import qualified Data.ByteString.Lazy as BSL
import Data.Typeable (Typeable)
import Data.Int
import Text.Printf

main :: IO ()
main = do
      putStrLn "Comparison of message lengths depending on serialization method"
      putStrLn "==============================================================="
      putStrLn ""
      putStrLn (ppr (measure "maxBound :: Int"  (maxBound :: Int)))
      putStrLn (ppr (measure "\"Hello\""        "Hello"))
      putStrLn (ppr (measure "100 chars lipsum" lipsum))
      putStrLn (ppr (measure "Right (Left \"Hello\") :: Either (Char, Int) (Either String (Maybe Integer))"
                             (Right (Left "Hello") :: Either (Char, Int) (Either String (Maybe Integer)))))


lipsum :: String
lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam\
         \ vitae lacinia tellus. Maecenas posuere."

data EncodeLengths = EL { info         :: String
                        , binary       :: Int64
                        , typedUntyped :: Int64
                        , typedHashed  :: Int64
                        , typedShown   :: Int64
                        , typedFull    :: Int64
                        }

-- | Prettyprint 'EncodeLengths'
ppr :: EncodeLengths -> String
ppr el = unlines [ info el
                 , printf "  Binary:        %d" (binary el)
                 , printf "  Typed/Untyped: %d (+%d, +%2.2f%%)"
                          (typedUntyped el)
                          (absolute (binary el) (typedUntyped el))
                          (percent  (binary el) (typedUntyped el))
                 , printf "  Typed/Hashed:  %d (+%d, +%2.2f%%)"
                          (typedHashed el)
                          (absolute (binary el) (typedHashed el))
                          (percent  (binary el) (typedHashed el))
                 , printf "  Typed/Shown:   %d (+%d, +%2.2f%%)"
                          (typedShown el)
                          (absolute (binary el) (typedShown el))
                          (percent  (binary el) (typedShown el))
                 , printf "  Typed/Full:    %d (+%d, +%2.2f%%)"
                          (typedFull el)
                          (absolute (binary el) (typedFull el))
                          (percent  (binary el) (typedFull el))
                 ]

-- | Calculate how much percent the first parameter deviates from the second.
percent :: Int64 -> Int64 -> Double
percent base x = let x'    = fromIntegral x
                     base' = fromIntegral base
                 in  abs ((x'-base')*100/base')


-- | Absolute deviation of the second from the first parameter
absolute :: Int64 -> Int64 -> Int64
absolute base x = x - base

-- | Measure the message lengths generated by different encodings.
measure :: (Binary a, Typeable a) => String -> a -> EncodeLengths
measure i x = EL { info = i
                 , binary       = BSL.length binary'
                 , typedUntyped = BSL.length typedUntyped'
                 , typedHashed  = BSL.length typedHashed'
                 , typedShown   = BSL.length typedShown'
                 , typedFull    = BSL.length typedFull'
                 }

      where

      binary'       = encode              x
      typedUntyped' = encodeTyped Untyped x
      typedHashed'  = encodeTyped Hashed  x
      typedShown'   = encodeTyped Shown   x
      typedFull'    = encodeTyped Full    x