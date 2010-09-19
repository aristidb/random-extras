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
)
where

import Control.Monad (liftM)
import Control.Monad.Random (MonadRandom, getRandomR)
import System.Random (Random)
import Data.List (mapAccumL)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Array.IArray as Arr
import Data.Sequence ((><), ViewL((:<)))

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
sample :: (MonadRandom m) => Int -> [a] -> m [a]
sample n = sampleSeq n . Seq.fromList

-- | Take a random sample from a sequence.
sampleSeq :: (MonadRandom m) => Int -> Seq.Seq a -> m [a]
sampleSeq n s = do
  samples <- getRandomRNums . take n . backsaw $ Seq.length s
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
choiceArray v = (Arr.!) v `liftM` getRandomR (Arr.bounds v)
