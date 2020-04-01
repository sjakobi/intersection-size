module Lib where

import Data.Functor.Const
import Data.IntMap
import Data.IntMap.Internal hiding (zipWithAMatched)
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

intersectionSizeInternal :: IntMap a -> IntMap b -> Int
intersectionSizeInternal = go
  where
    go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
      | shorter m1 m2  = merge1
      | shorter m2 m1  = merge2
      | p1 == p2       = (go l1 l2) + (go r1 r2)
      | otherwise      = 0
      where
        merge1 | nomatch p2 p1 m1  = 0
               | zero p2 m1        = go l1 t2
               | otherwise         = go r1 t2
        merge2 | nomatch p1 p2 m2  = 0
               | zero p1 m2        = go t1 l2
               | otherwise         = go t1 r2

    go t1'@(Bin _ _ _ _) t2'@(Tip k2' _) = merge0 t2' k2' t1'
      where
        merge0 t2 k2 (Bin p1 m1 l1 r1)
          | nomatch k2 p1 m1 = 0
          | zero k2 m1       = merge0 t2 k2 l1
          | otherwise        = merge0 t2 k2 r1
        merge0 _ k2 (Tip k1 _)
          | k1 == k2         = 1
          | otherwise        = 0
        merge0 _ _  Nil      = 0

    go (Bin _ _ _ _) Nil = 0

    go t1'@(Tip k1' _) t2' = merge0 t1' k1' t2'
      where
        merge0 t1 k1 (Bin p2 m2 l2 r2)
          | nomatch k1 p2 m2 = 0
          | zero k1 m2       = merge0 t1 k1 l2
          | otherwise        = merge0 t1 k1 r2
        merge0 _ k1 (Tip k2 _)
          | k1 == k2         = 1
          | otherwise        = 0
        merge0 _ _  Nil      = 0

    go Nil _ = 0
