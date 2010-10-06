{-# LANGUAGE FlexibleContexts #-}

{- |
Module       : Data.Random.Distribution.Uniform.Exclusive
Copyright    : 2010 Aristid Breitkreuz
License      : BSD3
Stability    : experimental
Portability  : portable

An uniform distribution that excludes the first parameter.
-}

module Data.Random.Distribution.Uniform.Exclusive
(
  Excludable(..)
, uniformExclusiveDist
, uniformExclusive
)
where
  
import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Internal.Fixed
import Data.Int
import Data.Word
import Data.Fixed

-- | A class for excluding discrete values. No change for floating point
-- values.
-- 
-- /Note:/ 'Uniform' is exclusive on the second argument for floating point
--         values, so 'Excludable' does not need to exclude anything for them.
class Excludable a where
    smaller :: a -> a
    bigger :: a -> a

-- | A uniform distribution that excludes the first parameter
-- , but includes the second.
-- 
-- /Note:/ 'Uniform' behaves the opposite way for floating point values.
uniformExclusiveDist :: (Excludable a, Ord a) => a -> a -> Uniform a
uniformExclusiveDist a b | c == EQ   = error "Invalid exclusive uniform distribution"
                         | c == LT   = Uniform b (bigger a)
                         | otherwise = Uniform (smaller a) b
    where c = compare a b

-- | A uniformly distributed random value that excludes the first parameter.
uniformExclusive :: (Distribution Uniform a, Excludable a, Ord a) => a -> a -> RVar a
uniformExclusive a b = rvar $ uniformExclusiveDist a b

instance Excludable Int where { smaller = pred; bigger = succ }
instance Excludable Int8 where { smaller = pred; bigger = succ }
instance Excludable Int16 where { smaller = pred; bigger = succ }
instance Excludable Int32 where { smaller = pred; bigger = succ }
instance Excludable Int64 where { smaller = pred; bigger = succ }
instance Excludable Word where { smaller = pred; bigger = succ }
instance Excludable Word8 where { smaller = pred; bigger = succ }
instance Excludable Word16 where { smaller = pred; bigger = succ }
instance Excludable Word32 where { smaller = pred; bigger = succ }
instance Excludable Word64 where { smaller = pred; bigger = succ }
instance Excludable Integer where { smaller = pred; bigger = succ }

instance Excludable Float where { smaller = id; bigger = id }
instance Excludable Double where { smaller = id; bigger = id }

instance Excludable Bool where { smaller = not; bigger = not }

instance HasResolution r => Excludable (Fixed r) where
    smaller = mkFixed . pred . unMkFixed
    bigger = mkFixed . succ . unMkFixed