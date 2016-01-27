{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Benchmark approx median
-- | Creator: Xiao Ling
-- | Created: 12/14/2015
-- | see    : http://www.serpentine.com/criterion/tutorial.html
-- | TODO   : move this to benchmark directory
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------


module BapproxMedian where 


import Data.List
import Data.Random
import Data.Conduit
import qualified Data.Conduit.List as Cl
import Criterion.Main


import Core
import Statistics
import ApproxMedian


main :: IO ()
main = return ()