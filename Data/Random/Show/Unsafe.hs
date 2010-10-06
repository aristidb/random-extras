{-# LANGUAGE TypeSynonymInstances #-}

{- |
Module       : Data.Random.Show.Unsafe
Copyright    : 2010 Aristid Breitkreuz
License      : BSD3
Stability    : experimental
Portability  : portable

Unsafely 'show' a 'RVar' by taking a random sample. Uses 'unsafePerformIO'.

Contains an instance of 'Show' for 'RVar'.
-}

module Data.Random.Show.Unsafe
()
where

import Data.Random.RVar
import Data.Random.Source.DevRandom
import System.IO.Unsafe

instance (Show a) => Show (RVar a) where
    show rv = show . unsafePerformIO $ runRVar rv DevURandom