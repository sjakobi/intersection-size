module Lib where

import Data.Functor.Const
import Data.IntMap
import Data.IntMap.Merge.Strict
import Data.Monoid (Sum(..))

intersectionSize :: IntMap a -> IntMap b -> Int
intersectionSize as bs =
    getSum $
    getConst $
    mergeA
        dropMissing
        dropMissing
        (zipWithAMatched (\_ _ _ -> Const (Sum 1)))
        as
        bs

intersectionSizeNaive :: IntMap a -> IntMap b -> Int
intersectionSizeNaive as bs = size (intersection as bs)
