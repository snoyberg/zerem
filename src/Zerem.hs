{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The primary set of functions for use with Zerem. This module is intended
-- to be imported qualified, e.g.:
--
-- > import qualified Zerem as Z
-- > import Zerem.Prelude
--
-- @since 0.1.0.0
module Zerem
  ( -- * Prelude
    module Zerem.Prelude
    -- * Producers
    -- ** Pure
  , enumFromTo
    -- ** Monadic
    -- ** I/O
    -- * Consumers
    -- ** Pure
  , foldl
  , sum
    -- ** Monadic
    -- ** I/O
    -- * Transformers
    -- ** Pure
    -- ** Monadic
    -- ** I/O
  ) where

import Zerem.Internal
import Zerem.Prelude
import Prelude (Num, Ord, Monad, (+), pure, otherwise, (>))

-- | Create a stream of values from low to high, inclusive.
--
-- @since 0.1.0.0
enumFromTo
  :: forall o m. (Num o, Ord o, Monad m)
  => o -- ^ low
  -> o -- ^ high
  -> Zerem o m ()
enumFromTo low high = Zerem next low
  where
    next :: o -> m (Step o o ())
    next curr
      | curr > high = pure (Done ())
      | otherwise =
          let !curr' = curr + 1
           in pure (Yield curr' curr)
{-# INLINE enumFromTo #-}

-- | Strict left fold
--
-- @since 0.1.0.0
foldl
  :: Monad m
  => (acc -> i -> acc)
  -> acc
  -> Zerem i m ()
  -> m acc
foldl f acc0 (Zerem next s0) =
  loop acc0 s0
  where
    loop !acc s = do
      step <- next s
      case step of
        Done () -> pure acc
        Skip s' -> loop acc s'
        Yield s' i -> loop (f acc i) s'
{-# INLINE foldl #-}

-- | Sum the values in the stream
--
-- @since 0.1.0.0
sum :: (Num i, Monad m) => Zerem i m () -> m i
sum = foldl (+) 0
{-# INLINE sum #-}
