import Gauge

import Data.List (foldl')
import Data.IntMap (IntMap)
import Lib
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

benchMany :: (IntMap a -> IntMap a -> Int) -> [IntMap a] -> Benchmarkable
benchMany f ms = nf (foldl' (+) 0 . map (uncurry f) . (\ms' -> zip ms' (drop 1 ms'))) ms

genIntMaps :: Int -> Int -> [IntMap ()]
genIntMaps seed size = let MkGen gen = arbitrary in gen (mkQCGen seed) size

main :: IO ()
main = defaultMain
    [ env (pure (genIntMaps 42 100000)) (\ms -> bgroup "intersectionSize"
          [ bench "naive" (benchMany intersectionSizeNaive ms)
          , bench "mergeA" (benchMany intersectionSize ms)
          , bench "internal" (benchMany intersectionSizeInternal ms)
          ])
    ]
          
