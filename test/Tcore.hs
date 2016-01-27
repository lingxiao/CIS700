{-# LANGUAGE OverloadedStrings, FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes, MultiParamTypeClasses, DeriveGeneric #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Core
-- | Creator: Xiao Ling
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Tcore where

import System.Random
import Test.QuickCheck

import Control.Monad
import Control.Monad.Trans

import Data.Random
import Data.Random.Source.Std

import Core



main :: IO ()
main = return ()

{-----------------------------------------------------------------------------
  Test 
------------------------------------------------------------------------------}

-- * Two tosses of same biased-coin results in two different results
-- * in expecation


-- * Two sequences of tosses of same-biased coin *always* results in same results

-- * Two sequences of tosses of different-biased coin results in same results
-- * in expectation


-- * P-biased coin is p-biased, in expectation


-- * Specifically, fair coin is fair, in expectation


