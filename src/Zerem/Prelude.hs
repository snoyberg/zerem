-- | Provides common data types for Zerem which can be imported unqualified.
-- See 'Zerem' for more information.
--
-- @since 0.1.0.0
module Zerem.Prelude
  ( Zerem
  , Identity (..)
  , ST
  , runST
  ) where

import Zerem.Internal (Zerem)
import Data.Functor.Identity (Identity (..))
import Control.Monad.ST (ST, runST)
