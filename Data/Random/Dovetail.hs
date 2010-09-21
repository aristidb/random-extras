{-# LANGUAGE FlexibleContexts #-}

{- |
Module       : Data.Random.Extras
Copyright    : 2010 Aristid Breitkreuz
License      : BSD3
Stability    : experimental
Portability  : portable

Functions for splitting a deck of cards like game players.

Decks are represented by 'Seq's, because these efficiently support the required operations.

[See:] Bayer, Diaconis /Trailing the Dovetail Shuffle to its Lair/ <http://projecteuclid.org/euclid.aoap/1177005705>
-}

module Data.Random.Dovetail
(
  -- * Splitting decks
  splitDeck
, generalizedSplitDeck
  -- * Riffling decks together
, riffleDecks
, generalizedRiffleDecks
  -- ** Inverse riffling
, inverseRiffleDecks
, generalizedInverseRiffleDecks
  -- * Dovetail shuffling
, dovetail
, generalizedDovetail
  -- ** Repeated dovetail shuffling
, dovetails
, generalizedDovetails
  -- ** Inverse dovetail shuffling
, inverseDovetail
, generalizedInverseDovetail
)
where
  
import Control.Applicative ((<$>))
import Control.Monad
import Data.Random.RVar  
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Uniform
import Data.Foldable (foldr1)
import Data.Sequence hiding (replicateM)
import Prelude hiding (null, length, splitAt, replicate, foldr1)

-- | Split a deck into two "roughly equal" halves.
splitDeck :: Seq a -> RVar (Seq a, Seq a)
splitDeck s = flip splitAt s <$> binomial (length s) (0.5 :: Double)

-- | Split a deck into /n/ "roughly equal" parts.
generalizedSplitDeck :: Int -> Seq a -> RVar [Seq a]
generalizedSplitDeck n s = split s <$> replicateM (n - 1) bin
    where bin = binomial (length s) (1 / fromIntegral n :: Double)
          split t []     = [t]
          split t (p:ps) = let (l, r) = splitAt p t in
                           l : split r ps

-- | Riffle two halves of a deck into one deck.
riffleDecks :: Seq a -> Seq a -> RVar (Seq a)
riffleDecks a b | null a = return b
                | null b = return a
                | otherwise = deterministicRiffle =<< uniform 1 len
    where lenA = length a
          lenB = length b
          len = lenA + lenB
          deterministicRiffle n | n <= lenA =
                                    let (a1 :< as) = viewl a in
                                    (a1 <|) <$> riffleDecks as b
                                | otherwise =
                                    let (b1 :< bs) = viewl b in
                                    (b1 <|) <$> riffleDecks a bs

-- | Riffle /n/ parts into one deck.
generalizedRiffleDecks :: [Seq a] -> RVar (Seq a)
generalizedRiffleDecks []     = return empty
generalizedRiffleDecks (x:xs) = riffleDecks x =<< generalizedRiffleDecks xs

-- | Perform an inverse riffle, i.e. letting the cards from a deck drop 
-- randomly into two heaps.
inverseRiffleDecks :: Seq a -> RVar (Seq a, Seq a)
inverseRiffleDecks s | null s    = return (empty, empty)
                     | otherwise = let (s1 :< ss) = viewl s in
                                   liftM2 (unriffle s1) (inverseRiffleDecks ss) (uniform False True)
    where unriffle a (l, r) left = case left of
                                     True  -> (a <| l, r)
                                     False -> (l, a <| r)

-- | Perform an inverse riffle, i.e. letting the cards from a deck drop
-- randomly into /n/ heaps.
generalizedInverseRiffleDecks :: Int -> Seq a -> RVar (Seq (Seq a))
generalizedInverseRiffleDecks n s | null s    = return $ replicate n empty
                                  | otherwise = let (s1 :< ss) = viewl s in
                                                liftM2 (unriffle s1) (generalizedInverseRiffleDecks n ss) (uniform 0 (n - 1))
    where unriffle a t i = adjust (a <|) i t

-- | Dovetail shuffle a deck, i.e. split the deck with splitDeck and riffle 
-- the resulting halves with 'riffleDecks'.
dovetail :: Seq a -> RVar (Seq a)
dovetail s = uncurry riffleDecks =<< splitDeck s

-- | Dovetail shuffle a deck (generalized), i.e. split the deck into /n/ parts
-- and riffle them back together.
generalizedDovetail :: Int -> Seq a -> RVar (Seq a)
generalizedDovetail n s = generalizedRiffleDecks =<< generalizedSplitDeck n s

-- | Dovetail shuffle a deck repeatedly for /n/ times.
dovetails :: Int -> Seq a -> RVar (Seq a)
dovetails n s | n > 0     = dovetail =<< dovetails (n - 1) s
              | otherwise = return s

-- | Dovetail shuffle a deck repeatedly for /shuffles/ times, 
-- using /parts/ parts.
-- 
-- /Invocation:/ @generalizedDovetails shuffles parts deck@
generalizedDovetails :: Int -> Int -> Seq a -> RVar (Seq a)
generalizedDovetails shuffles parts s | shuffles > 0 = generalizedDovetail parts =<< generalizedDovetails (shuffles - 1) parts s
                                      | otherwise    = return s

-- | Perform an inverse dovetail shuffle, i.e. letting the cards from 
-- a deck drop randomly into two heaps and then stack these heaps.
inverseDovetail :: Seq a -> RVar (Seq a)
inverseDovetail s = uncurry (><) <$> inverseRiffleDecks s

-- | Performan a generalized inverse dovetail shuffle, i.e. letting the cards
-- from a deck drop randomly into /n/ heaps and then stack them back together.
generalizedInverseDovetail :: Int -> Seq a -> RVar (Seq a)
generalizedInverseDovetail n s | null s    = return empty
                               | otherwise = foldr1 (><) <$> generalizedInverseRiffleDecks n s
