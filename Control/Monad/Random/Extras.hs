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
  -- ** Choice
, choiceExtract
, choiceExtractSeq
, choice
, choiceSeq
)
where

import Control.Monad (MonadPlus, liftM)
import Control.Monad.Loops (unfoldrM')
import Control.Monad.Random (MonadRandom, getRandomR)
import qualified Data.Sequence as Seq
import Data.Sequence ((><), ViewL((:<)))

extract :: [a] -> Int -> Maybe (a, [a])
extract s i | null r    = Nothing
            | otherwise = Just (b, a ++ c)
    where (a, r) = splitAt i s
          (b : c) = r

extractSeq :: Seq.Seq a -> Int -> Maybe (a, Seq.Seq a)
extractSeq s i | Seq.null r = Nothing
               | otherwise  = Just (b, a >< c)
    where (a, r) = Seq.splitAt i s 
          (b :< c) = Seq.viewl r

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
shuffleSeq :: (MonadRandom m, MonadPlus f) => Seq.Seq a -> m (f a)
shuffleSeq = unfoldrM' choiceExtractSeq

-- | Randomly choose and extract an element from a list.
--
-- Complexity: O(n)
choiceExtract :: (MonadRandom m) => [a] -> m (Maybe (a, [a]))
choiceExtract [] = return Nothing
choiceExtract xs = extract xs `liftM` getRandomR (0, length xs - 1)
                    
-- | Randomly choose and extract an element from a sequence.
--
-- Complexity: O(log n)
choiceExtractSeq :: (MonadRandom m) => Seq.Seq a -> m (Maybe (a, Seq.Seq a))
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
