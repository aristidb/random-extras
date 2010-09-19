module Control.Monad.Random.Extras
(
  shuffle
, shuffle'
, choice
)
where

import Control.Monad (liftM)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.Random (MonadRandom, getRandomR)
import qualified Data.Sequence as Seq
import Data.Sequence ((><), ViewL((:<)))

extract :: Seq.Seq a -> Int -> Maybe (a, Seq.Seq a)
extract s i | Seq.null r = Nothing
            | otherwise  = Just (b, a >< c)
  where (a, r) = Seq.splitAt i s 
        (b :< c) = Seq.viewl r

-- | Shuffle a list randomly. The method is based on Oleg Kiselyov's 
-- "perfect shuffle" <http://okmij.org/ftp/Haskell/perfect-shuffle.txt>,
-- but much simpler because it uses existing data structures. The efficiency
-- of both methods should be comparable.
shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffle' . Seq.fromList

-- | Shuffle a Data.Sequence randomly. This is being used by 'shuffle',
-- so it logically uses the same method.
shuffle' :: (MonadRandom m) => Seq.Seq a -> m [a]
shuffle' = unfoldrM step
    where step s = extract s `liftM` getRandomR range
              where range = (0, max (Seq.length s - 1) 0)

-- | Select a random element from a list. Complexity: O(n).
choice :: (MonadRandom m) => [a] -> m a
choice [] = error "Control.Monad.Random.Extras.choice: empty list"
choice xs = (xs !!) `liftM` getRandomR (0, length xs - 1)