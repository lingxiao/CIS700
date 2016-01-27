{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


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
  , count
  , count'
  ) where

import Data.Random
import Data.Conduit
import Data.List.Split
import qualified Data.Conduit.List as Cl
import Control.Monad.Identity
import System.Environment


import Core
import Statistics


{-----------------------------------------------------------------------------
    I. Morris Algorithm list of counter
------------------------------------------------------------------------------}

-- * Count the number of items in `as` to within `eps` of actual
-- * with confidence `delta`
morris :: Eps -> Delta -> [a] -> IO Counter
morris e d as = runRVar (goMorris e d as) StdRandom

goMorris :: Eps -> Delta -> [a] -> RVar Counter
goMorris e d as = medianOfMeans $ Cl.sourceList as $$ count $ cs
  where cs = replicate (t*m) 0
        t  = round $ 1/(e^2*d)  :: Int          
        m  = round . log $ 1/d  
        medianOfMeans = fmap median' . (fmap . fmap) mean' . fmap (chunksOf t) 


count :: [Counter] -> Sink a RVar [Counter]
count cs = (\c -> 2^(round c) - 1) `ffmap` Cl.foldM (\cs _ -> traverse incr cs) cs
  where ffmap = fmap . fmap


{-----------------------------------------------------------------------------
    II. Morris Algorithm list of list of counter
------------------------------------------------------------------------------}

-- * Count the number of items in `as` to within `eps` of actual
-- * with confidence `delta`
morris' :: Eps -> Delta -> [a] -> IO Counter
morris' e d as = runRVar (goMorris' e d as) StdRandom

-- * 160000
-- * Run on stream inputs `as` for t independent trials for `t = 1/eps^2 * d`, 
-- * and `m` times in parallel, for `m = log(1/d)` and take the median
goMorris' :: Eps -> Delta -> [a] -> RVar Counter
goMorris' e d as = medianOfMeans $ Cl.sourceList as $$ count' $ ccs
      where
        medianOfMeans = fmap median' . (fmap . fmap) mean' 
        ccs           = replicate m $ replicate t 0
        t             = round $ 1/(e^2*d)            
        m             = round . log $ 1/d  


-- * Given an m-long list `ccs` of lists (each of which is t-lengthd) of counters,
-- * consume the stream and output result
count' :: [[Counter]] -> Sink a RVar [[Counter]]
count' ccs = (\x -> 2^(round x) - 1) `fffmap` Cl.foldM (\xs _ -> incrs' xs) ccs
  where fffmap = fmap . fmap . fmap

-- * given a list of list of counters toss a coin for each counter and incr
-- * this can be flattened
incrs' :: [[Counter]] -> RVar [[Counter]]
incrs' = sequence . fmap (sequence . fmap incr)



{-----------------------------------------------------------------------------
    III. Utils
------------------------------------------------------------------------------}


-- * Increment a counter `x` with probability 1/2^x
incr :: Counter -> RVar Counter
incr x = do
  h <- toss . coin $ 0.5^(round x)
  return $ if isHead h then (seq () succ x) else seq () x


mean', median' :: (Floating a, Ord a, RealFrac a) => [a] -> Float
mean'   = fromIntegral . round . mean
median' = fromIntegral . round . median



{-----------------------------------------------------------------------------
    IV. Naive Implementation
------------------------------------------------------------------------------}

-- * 2x slower than morris'
-- * Run on stream inputs `xs` for t independent trials for `t = 1/eps`, 
-- * and `m` times in parralell where `m = 1/(e^2 * d)`, take the median
morrisNaive :: Eps -> Delta -> [a] -> IO Counter
morrisNaive e d = fmap median' . replicateM m . morrisB t 
  where (t,m) = (round $ 1/(e^2*d), round $ 1/d)


-- * Run Morris beta on stream inputs `xs` for `t` independent trials and average
morrisB :: Int -> [a] -> IO Counter
morrisB t =  fmap mean' . replicateM t . morrisA

-- * Run Morris alpha on stream inputs `xs`
morrisA :: [a] -> IO Counter
morrisA xs = flip runRVar StdRandom $ Cl.sourceList xs $$ alpha


-- * A step in morris Algorithm alpha
alpha :: Sink a RVar Counter
alpha = (\x -> 2^(round x) - 1) <$> Cl.foldM (\x _ -> incr x) 0














