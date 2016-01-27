{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Test approx median
-- | Creator: Xiao Ling
-- | Created: 12/14/2015
-- | see    : http://www.serpentine.com/criterion/tutorial.html
-- |        : http://blog.functorial.com/posts/2012-08-04-Testing-Random-Properties-With-Type-Classes.html
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------


module TcountMinSketch where 


import Control.Monad.Random.Class
import Control.Monad.Random
import Control.Monad.State
import Control.Arrow

import Data.List
import Data.Matrix
import Data.Conduit
import qualified Data.Conduit.List as Cl
import qualified Data.Matrix as M
import qualified Data.Map as Mp

import Criterion.Main
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers


import Core
import Utils
import CountMinSketch

{-----------------------------------------------------------------------------
    Test Correctness
------------------------------------------------------------------------------}

main :: IO ([Int],[Int],[Int])
main = do
    --n  <- getRandomR (1,100 :: Int)
    let n = 50
    xs <- take 10000 <$> getRandomRs (1, n :: Int)
    let as = fmap snd . dfreq $ xs
    g <- test (sketch' ed n) xs
    let bs = g <$> [1..n]
    return (xs,as,bs)


{-----------------------------------------------------------------------------
    Deterministic Algorithm
------------------------------------------------------------------------------}

dfreq :: Ord a => [a] -> [(a,Int)] 
dfreq = fmap (\(a,b) -> (b,a)) . fmap (length &&& head) . group . sort 

-- * euclidian distance between
dist :: (Num a,Floating a) => [Int] -> [Int] -> a
dist as = sqrt . fromIntegral . foldr (+) 0 . fmap (^2) . zipWith (-) as
















