{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Test Morris
-- | Creator: Xiao Ling
-- | Created: 12/8/2015
-- | see    : https://github.com/snoyberg/conduit
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Tmorris where

import Data.List
import Data.Random
import Data.Conduit
import Data.List.Split
import qualified Data.Conduit.List as Cl

import Control.Monad.Identity

import Criterion.Main


import Core
import Statistics
import Morris


main :: IO ()
main = return ()