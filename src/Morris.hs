{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, ConstraintKinds, ExistentialQuantification, GADTs, RankNTypes, MultiParamTypeClasses, RankNTypes, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : First attempt at approximate counting
-- | Creator: Xiao Ling
-- | Created: 12/08/2015
-- | TODO   : test standard deviation of alpha, beta, and final version
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Morris (

    morris
  , morris'

  , morris1
  , morris1'

  ) where

import Control.Monad.Random
import Control.Monad.Random.Class

import Data.Conduit
import Data.List.Split
import qualified Data.Conduit.List as Cl


import Core
import Statistics


{-----------------------------------------------------------------------------
    I. Morris Algorithm list of counter
------------------------------------------------------------------------------}

---- * Count the number of items in `as` to within `eps` of actual
---- * with confidence `delta`
morris' :: EpsDelta -> Batch a IO Counter
morris' ed = morris ed `using` evalRandIO

morris :: MonadRandom m => EpsDelta -> Streaming a m Counter
morris (ED e d) = medianOfMeans $ go cs
  where cs            = replicate (t*m) 0
        t             = round $ 1/(e^2*d)  :: Int          
        m             = round . log $ 1/d  
        go cs         = (\c -> 2^(round c) - 1) `ffmap` Cl.foldM (\cs _ -> traverse incr cs) cs
        ffmap         = fmap . fmap
        medianOfMeans = fmap median' . (fmap . fmap) mean' . fmap (chunksOf t) 


{-----------------------------------------------------------------------------
    II. Morris Algorithm list of list of counter
------------------------------------------------------------------------------}

---- * Count the number of items in `as` to within `eps` of actual
---- * with confidence `delta`
morris1' :: EpsDelta -> Batch a IO Counter
morris1' ed = morris1 ed `using` evalRandIO

-- * Run on stream inputs `as` for t independent trials for `t = 1/eps^2 * d`, 
-- * and `m` times in parallel, for `m = log(1/d)` and take the median
morris1 :: MonadRandom m => EpsDelta -> Streaming a m Counter
morris1 (ED e d) = medianOfMeans $ go ccs
      where
        medianOfMeans = fmap median' . (fmap . fmap) mean' 
        ccs           = replicate m $ replicate t 0
        t             = round $ 1/(e^2*d)            
        m             = round . log $ 1/d  
        go ccs        = (\x -> 2^(round x) - 1) `fffmap` Cl.foldM (\xs _ -> incrs' xs) ccs
        fffmap        = fmap . fmap . fmap

-- * given a list of list of counters toss a coin for each counter and incr
-- * this can be flattened
incrs' :: MonadRandom m => [[Counter]] -> m [[Counter]]
incrs' = sequence . fmap (sequence . fmap incr)


{-----------------------------------------------------------------------------
    III. Utils
------------------------------------------------------------------------------}


-- * Increment a counter `x` with probability 1/2^x
incr :: MonadRandom m => Counter -> m Counter
incr x = do
  h <- toss . coin $ 0.5^(round x)
  return $ if isHead h then (seq () succ x) else seq () x


mean', median' :: (Floating a, Ord a, RealFrac a) => [a] -> Float
mean'   = fromIntegral . round . mean
median' = fromIntegral . round . median










