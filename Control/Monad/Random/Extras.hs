{- |
Module       : Control.Monad.Random.Extras
Copyright    : 2010 Aristid Breitkreuz
License      : BSD3
Stability    : experimental
Portability  : portable

Additional monadic random functions, based on 'MonadRandom'.

-}


module Control.Monad.Random.Extras
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
, choiceSeq
, choiceArray
  -- ** Choices
, choices
, choicesArray
)
where

import Control.Monad (liftM)
import Control.Monad.Random (MonadRandom, getRandomR, getRandomRs)
import System.Random (Random)
import Data.List (mapAccumL)
import Data.Maybe (fromJust)
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

getRandomR' :: (MonadRandom m, Random a) => a -> a -> m a
getRandomR' = curry getRandomR

getRandomRNums :: (MonadRandom m, Random a, Num a) => [a] -> m [a]
getRandomRNums = mapM (getRandomR' 0)

backsaw :: Int -> [Int]
backsaw n = [n - 1, n - 2 .. 0]
          
-- Shuffling

-- | Shuffle a list randomly. The method is based on Oleg Kiselyov's 
-- /perfect shuffle/ <http://okmij.org/ftp/Haskell/perfect-shuffle.txt>,
-- but much simpler because it uses existing data structures. The efficiency
-- of both methods should be comparable.
--
-- Complexity: O(n * log n)
shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffleSeq . Seq.fromList

-- | Shuffle a sequence randomly. This is being used by 'shuffle',
-- so it logically uses the same method.
--
-- Complexity: O(n * log n)
shuffleSeq :: (MonadRandom m) => Seq.Seq a -> m [a]
shuffleSeq s = do
  samples <- getRandomRNums . backsaw $ Seq.length s
  return (shuffleSeq' s samples)

shuffleSeq' :: Seq.Seq a -> [Int] -> [a]
shuffleSeq' = snd .: mapAccumL (fromJust .: extractSeq)

-- Sampling

-- | Take a random sample from a list.
-- 
-- Complexity: O(n + m * log n)
sample :: (MonadRandom m) => Int -> [a] -> m [a]
sample m = sampleSeq m . Seq.fromList

-- | Take a random sample from a sequence.
-- 
-- Complexity: O(m * log n)
sampleSeq :: (MonadRandom m) => Int -> Seq.Seq a -> m [a]
sampleSeq m s = do
  samples <- getRandomRNums . take m . backsaw $ Seq.length s
  return (shuffleSeq' s samples)

-- Choice

-- | Randomly choose and extract an element from a list.
--
-- Complexity: O(n)
choiceExtract :: (MonadRandom m) => [a] -> m (Maybe ([a], a))
choiceExtract [] = return Nothing
choiceExtract xs = extract xs `liftM` getRandomR (0, length xs - 1)
                    
-- | Randomly choose and extract an element from a sequence.
--
-- Complexity: O(log n)
choiceExtractSeq :: (MonadRandom m) => Seq.Seq a -> m (Maybe (Seq.Seq a, a))
choiceExtractSeq s | Seq.null s = return Nothing
                   | otherwise  = extractSeq s `liftM` getRandomR (0, Seq.length s - 1)

-- | Select a random element from a list.
--
-- Complexity: O(n).
choice :: (MonadRandom m) => [a] -> m a
choice [] = error "Control.Monad.Random.Extras.choice: empty list"
choice xs = (xs !!) `liftM` getRandomR (0, length xs - 1)

-- | Select a random element from a sequence.
--
-- Complexity: O(log n).
choiceSeq :: (MonadRandom m) => Seq.Seq a -> m a
choiceSeq s | Seq.null s = error "Control.Monad.Random.Extras.choiceSeq: empty sequence"
            | otherwise  = Seq.index s `liftM` getRandomR (0, Seq.length s - 1)

-- | Select a random element from an array.
--
-- Complexity: O(1).
choiceArray :: (MonadRandom m, Arr.IArray arr a, Arr.Ix i, Random i) => arr i a -> m a
choiceArray v = (v !) `liftM` getRandomR (Arr.bounds v)

-- Choices

-- | Select /m/ random elements from a list.
--
-- Complexity: O(m)
choices :: (MonadRandom m) => Int -> [a] -> m [a]
choices m xs = choicesArray m (Data.Array.listArray (1, length xs) xs)

-- | Select /m/ random elements from an array.
--
-- Complexity: O(m)
choicesArray :: (MonadRandom m, Arr.IArray arr a, Arr.Ix i, Random i) => Int -> arr i a -> m [a]
choicesArray m v = map (v !) `liftM` take m `liftM` getRandomRs (Arr.bounds v)
