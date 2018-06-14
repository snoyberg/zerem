{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Raw, internal representations for Zerem. This API is unstable, your code
-- may breeak if you use this module!
--
-- @since 0.1.0.0
module Zerem.Internal
  ( Step (..)
  , Zerem (..)
  ) where

-- FIXME is there any good motivation for keeping the r parameter, or should we
-- drop it?
data Step s o r where
  Yield :: s -> o -> Step s o r
  Skip :: s -> Step s o r
  Done :: r -> Step s o r
  deriving Functor

data Zerem o m r where
  Zerem
    :: (s -> m (Step s o r))
    -> s
    -> Zerem o m r
deriving instance Functor m => Functor (Zerem o m)
