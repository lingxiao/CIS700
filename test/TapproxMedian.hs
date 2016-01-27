{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Test approx median
-- | Creator: Xiao Ling
-- | Created: 12/14/2015
-- | see    : http://www.serpentine.com/criterion/tutorial.html
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------


module TapproxMedian where 


import Control.Monad.Random.Class
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Reader

import Data.Conduit
import qualified Data.Conduit.List as Cl



import Core
import Statistics
import ApproxMedian


{-----------------------------------------------------------------------------
  Benchmark  counter vs counter'
------------------------------------------------------------------------------}

main :: IO ()
main = return ()



