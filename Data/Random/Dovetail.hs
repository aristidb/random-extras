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
where
  
import Control.Applicative ((<$>))
import Control.Monad
import Data.Random.RVar  
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Uniform
import Data.Sequence
import Prelude hiding (null, length, splitAt)

-- | Split a deck into two halves.
splitDeck :: Seq a -> RVar (Seq a, Seq a)
splitDeck s = flip splitAt s <$> binomial (length s) (0.5 :: Double)

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

-- | Perform an "inverse riffle", i.e. letting the cards from a deck drop 
-- randomly into two heaps.
inverseRiffleDecks :: Seq a -> RVar (Seq a, Seq a)
inverseRiffleDecks s | null s    = return (empty, empty)
                     | otherwise = let (s1 :< ss) = viewl s in
                                   liftM2 (unriffle s1) (inverseRiffleDecks ss) (uniform False True)
    where unriffle a (l, r) left = case left of
                                     True  -> (a <| l, r)
                                     False -> (l, a <| r)

-- | Dovetail shuffle a deck, i.e. split the deck with splitDeck and riffle 
-- the resulting halves with 'riffleDecks'.
dovetail :: Seq a -> RVar (Seq a)
dovetail s = uncurry riffleDecks =<< splitDeck s

-- | Dovetail shuffle a deck repeatedly for /n/ times.
dovetails :: Int -> Seq a -> RVar (Seq a)
dovetails n s | n > 0     = dovetail =<< dovetails (n - 1) s
              | otherwise = return s

-- | Performan an "inverse dovetail shuffle", i.e. letting the cards from 
-- a deck drop randomly into two heaps and then stack these heaps.
inverseDovetail :: Seq a -> RVar (Seq a)
inverseDovetail s = uncurry (><) <$> inverseRiffleDecks s