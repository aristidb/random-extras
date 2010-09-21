{-# LANGUAGE FlexibleContexts #-}

{- |
Module       : Data.Random.Extras
Copyright    : 2010 Aristid Breitkreuz
License      : BSD3
Stability    : experimental
Portability  : portable

Functions for shuffling elements according to weights.
-}

module Data.Random.Shuffle.Weighted
where
  
import Control.Applicative ((<$>))
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import qualified Data.Map as M

moduleError :: String -> String -> a
moduleError n s = error $ "Data.Random.Shuffle.Weighted." ++ n ++ ": " ++ s
  
weightedChoiceExtractCDF :: (Num w, Ord w, Distribution Uniform w) => M.Map w a -> RVar (M.Map w a, a)
weightedChoiceExtractCDF m | M.null m  = moduleError "weightedChoiceExtractCDF" "empty map"
                           | otherwise = extract <$> uniform 0 wmax
    where Just ((wmax, _), _) = M.maxViewWithKey m
          extract w = (a `M.union` c, b)
              where (a, r) = M.split w m
                    Just (b, c) = M.minView r

weightedShuffleCDF :: (Num w, Ord w, Distribution Uniform w) => M.Map w a -> RVar [a]
weightedShuffleCDF m | M.null m  = return []
                     | otherwise = weightedChoiceExtractCDF m >>= \(m', a) -> (a:) <$> weightedShuffleCDF m'

weightedShuffle :: (Num w, Ord w, Distribution Uniform w) => [(w, a)] -> RVar [a]
weightedShuffle = weightedShuffleCDF . M.fromAscList . scanl1 (\(w1, _) (w2, x) -> (w1 + w2, x))

weightedSampleCDF :: (Num w, Ord w, Distribution Uniform w) => Int -> M.Map w a -> RVar [a]
weightedSampleCDF n m | M.null m || n <= 0 = return []
                      | otherwise          = weightedChoiceExtractCDF m >>= \(m', a) -> (a:) <$> weightedSampleCDF (n - 1) m'

weightedSample :: (Num w, Ord w, Distribution Uniform w) => Int -> [(w, a)] -> RVar [a]
weightedSample n = weightedSampleCDF n . M.fromAscList . scanl1 (\(w1, _) (w2, x) -> (w1 + w2, x))