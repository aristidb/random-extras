{-# LANGUAGE FlexibleContexts #-}

{- |
Module       : Control.Monad.Random.Extras
Copyright    : 2010 Aristid Breitkreuz
License      : BSD3
Stability    : experimental
Portability  : portable

Additional monadic random functions, based on random-fu.
-}

module Data.Random.Extras
(
  -- * Random functions
  -- ** Shuffling
  shuffle
, shuffleSeq
  -- ** Sampling
, sample
, sampleSeq
  -- ** Choice
, choiceExtract
, choiceExtractSeq
, choice
, safeChoice
, iterativeChoice
, choiceSeq
, safeChoiceSeq
, choiceArray
  -- ** Choices
, choices
, safeChoices
, choicesArray
)
where

import Control.Monad
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence ((><), ViewL((:<)))
import qualified Data.Array.IArray as Arr
import qualified Data.Array
import Data.Array.IArray ((!))

(.:) :: (c -> c') -> (a -> b -> c) -> (a -> b -> c')
(.:) = (.).(.)

extract :: [a] -> Int -> Maybe ([a], a)
extract s i | null r    = Nothing
            | otherwise = Just (a ++ c, b)
    where (a, r) = splitAt i s
          (b : c) = r

extractSeq :: Seq.Seq a -> Int -> Maybe (Seq.Seq a, a)
extractSeq s i | Seq.null r = Nothing
               | otherwise  = Just (a >< c, b)
    where (a, r) = Seq.splitAt i s 
          (b :< c) = Seq.viewl r

backsaw :: Int -> [Int]
backsaw n = [n - 1, n - 2 .. 0]
          
-- Shuffling

-- | Shuffle a list randomly. The method is based on Oleg Kiselyov's 
-- /perfect shuffle/ <http://okmij.org/ftp/Haskell/perfect-shuffle.txt>,
-- but much simpler because it uses existing data structures. The efficiency
-- of both methods should be comparable.
--
-- /Complexity:/ O(n * log n), where /n/ is the length of the input list.
shuffle :: [a] -> RVar [a]
shuffle = shuffleSeq . Seq.fromList

-- | Shuffle a sequence randomly. This is being used by 'shuffle',
-- so it logically uses the same method.
--
-- /Complexity:/ O(n * log n), where /n/ is the length of the input sequence.
shuffleSeq :: Seq.Seq a -> RVar [a]
shuffleSeq s = do
  samples <- mapM (uniform 0) . backsaw $ Seq.length s
  return (shuffleSeq' s samples)

shuffleSeq' :: Seq.Seq a -> [Int] -> [a]
shuffleSeq' = snd .: mapAccumL (fromJust .: extractSeq)

-- Sampling

-- | Take a random sample from a list.
-- 
-- /Complexity:/ O(n + m * log n), where /n/ is the length of the input list 
-- and /m/ is the sample size.
sample :: Int -> [a] -> RVar [a]
sample m = sampleSeq m . Seq.fromList

-- | Take a random sample from a sequence.
-- 
-- /Complexity:/ O(m * log n), where /n/ is the length of the input sequence 
-- and /m/ is the sample size.
sampleSeq :: Int -> Seq.Seq a -> RVar [a]
sampleSeq m s = do
  samples <- mapM (uniform 0) . take m . backsaw $ Seq.length s
  return (shuffleSeq' s samples)

-- Choice

-- | Randomly choose and extract an element from a list.
-- 
-- /Complexity:/ O(n), where /n/ is the length of the input list.
choiceExtract :: [a] -> Maybe (RVar ([a], a))
choiceExtract [] = Nothing
choiceExtract xs = Just $ (fromJust . extract xs) `liftM` uniform 0 (length xs - 1)
                    
-- | Randomly choose and extract an element from a sequence.
-- 
-- /Complexity:/ O(log n), where /n/ is the length of the input sequence.
choiceExtractSeq :: Seq.Seq a -> Maybe (RVar (Seq.Seq a, a))
choiceExtractSeq s | Seq.null s = Nothing
                   | otherwise  = Just $ (fromJust . extractSeq s) `liftM` uniform 0 (Seq.length s - 1)

-- | Select a random element from a list.
-- 
-- /Partial function:/ This function is only defined on non-empty lists.
-- 
-- /Complexity:/ O(n), where /n/ is the length of the input list.
choice :: [a] -> RVar a
choice [] = error "Control.Monad.Random.Extras.choice: empty list"
choice xs = (xs !!) `liftM` uniform 0 (length xs - 1)

-- | Safely select a random element from a list.
-- 
-- /Complexity:/ O(n), where /n/ is the length of the input list.
safeChoice :: [a] -> Maybe (RVar a)
safeChoice [] = Nothing
safeChoice xs = Just $ choice xs

-- | Select a random element from a list, traversing the list only once.
-- 
-- /Partial function:/ This function is only defined on non-empty lists
--                     with a length below ('maxBound' + 1 :: Int).
-- 
-- /Complexity:/ O(n), where /n/ is the length of the input list.
iterativeChoice :: [a] -> RVar a
iterativeChoice xs = fst `liftM` foldl' stepM (return start) xs
    where stepM x y = x >>= step y
          step offered (old, n) = do
            i <- uniform 0 n
            let new | i == 0    = offered
                    | otherwise = old
            return $! new `seq` (new, n + 1)
          start = (err, 0 :: Int)
          err = error "Control.Monad.Random.Extras.iterativeChoice: empty list"

-- | Select a random element from a sequence.
-- 
-- /Partial function:/ This function is only defined on non-empty sequences.
-- 
-- /Complexity:/ O(log n), where /n/ is the length of the input sequence.
choiceSeq :: Seq.Seq a -> RVar a
choiceSeq s | Seq.null s = error "Control.Monad.Random.Extras.choiceSeq: empty sequence"
            | otherwise  = Seq.index s `liftM` uniform 0 (Seq.length s - 1)
                           
-- | Safely select a random element from a sequence.
-- 
-- /Complexity:/ O(log n), where /n/ is the length of the input sequence.
safeChoiceSeq :: Seq.Seq a -> Maybe (RVar a)
safeChoiceSeq s | Seq.null s = Nothing
                | otherwise  = Just $ choiceSeq s

-- | Select a random element from an array.
-- 
-- /Complexity:/ O(1).
choiceArray :: (Arr.IArray arr a, Arr.Ix i, Distribution Uniform i) => arr i a -> RVar a
choiceArray v = (v !) `liftM` uncurry uniform (Arr.bounds v)

-- Choices

-- | A stream of random elements from a list.
-- 
-- /Partial function:/ This function is only defined on non-empty lists.
-- 
-- /Complexity:/ O(n) base and O(1) per element.
choices :: Int -> [a] -> RVar [a]
choices _ [] = error "Control.Monad.Random.Extras.choices: empty list"
choices n xs = choicesArray n $ Data.Array.listArray (1, length xs) xs

-- | Safely get a stream of random elements from a list.
-- 
-- /Complexity:/ O(n) base and O(1) per element, where /n/ is the length of 
-- the input list.
safeChoices :: Int -> [a] -> Maybe (RVar [a])
safeChoices _ [] = Nothing
safeChoices n xs = Just $ choices n xs

-- | A stream of random elements from an array.
--
-- /Complexity:/ O(1) per element.
choicesArray :: (Arr.IArray arr a, Arr.Ix i, Distribution Uniform i) => Int -> arr i a -> RVar [a]
choicesArray n v = map (v !) `liftM` replicateM n (uncurry uniform (Arr.bounds v))