{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Fequency Moments
-- | Creator: Xiao Ling
-- | Created: 12/17/2015
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module FeqMoments where

import Prelude hiding (replicate)
import Control.Monad.Random.Class
import Control.Monad.Random
import Control.Monad.State

import Data.Conduit
import Data.Foldable (toList)
import qualified Data.Sequence as S
import qualified Data.Conduit.List as Cl
import Data.Sequence (Seq,(|>),update,empty)

import Core
import Statistics


{-----------------------------------------------------------------------------
  Types 
------------------------------------------------------------------------------}






{-----------------------------------------------------------------------------
  Approximate Median 
------------------------------------------------------------------------------}










