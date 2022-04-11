{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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
import System.IO.Unsafe
import System.Random.Stateful

instance (Show a) => Show (RVar a) where
    show rv = show . unsafePerformIO $ runRVar rv globalStdGen
