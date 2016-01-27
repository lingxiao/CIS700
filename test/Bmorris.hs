{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Bench mark morris 
-- | Creator: Xiao Ling
-- | Created: 12/8/2015
-- | see    : https://github.com/snoyberg/conduit
-- | TODO   : move this to benchmark directory
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Bmorris where

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


{-----------------------------------------------------------------------------
  Benchmark  counter vs counter'
  
    Benchmark results: it's unclear whether counter is faster than counter'
    due to noise: 

    benchmarking count vs count'/count 500
      time                 6.527 s    (-1.716 s .. 19.59 s)
                           0.692 R²   (0.283 R² .. 1.000 R²)
      mean                 4.232 s    (2.136 s .. 5.686 s)
      std dev              2.186 s    (0.0 s .. 2.519 s)
      variance introduced by outliers: 74% (severely inflated)

    benchmarking count vs count'/count' 500
      time                 3.315 s    (-6.152 s .. 16.65 s)
                           0.357 R²   (0.239 R² .. 1.000 R²)
      mean                 5.474 s    (2.826 s .. 6.971 s)
      std dev              2.351 s    (0.0 s .. 2.592 s)
      variance introduced by outliers: 74% (severely inflated)

    The variance actually went up as # of counters increased

------------------------------------------------------------------------------}


main :: IO ()
main = defaultMain . return $ bgroup "flat vs nested morris" [
        bench "flat list e=d=0.05"   . nfIO $ morris 0.05 0.05 [1..10000]
      , bench "nested list e=d=0.05" . nfIO $ morris' 0.05 0.05 [1..10000]
  ]

_count cs  = runRVar (Cl.sourceList [1..1000] $$ count cs) StdRandom
_count' cs = runRVar (Cl.sourceList [1..1000] $$ count' cs) StdRandom


(t1,m1) = (10,3)   :: (Int,Int)
(t2,m2) = (100,3)  :: (Int,Int)
(t3,m3) = (8000,3)  :: (Int,Int)

cs' :: Num a => Int -> Int -> [[Counter]]
cs' t m  = replicate m $ replicate t 0  

cs :: Num a => Int -> Int -> [Counter]
cs t m = replicate (t*m) 0            

-- * conclude: morris 2x faster than morris', not surprisingly
--main :: IO ()
--main = defaultMain . return $ bgroup "morris vs morris'" [
--        bench "morris 10"  . nfIO $ morris  0.5 0.5 [1..10]
--      , bench "morris' 10" . nfIO $ morris' 0.5 0.5 [1..10]

--      , bench "morris 100"  . nfIO $ morris  0.5 0.5 [1..100]
--      , bench "morris' 100" . nfIO $ morris' 0.5 0.5 [1..100]

--      , bench "morris 1000"  . nfIO $ morris  0.5 0.5 [1..1000]
--      , bench "morris' 1000" . nfIO $ morris' 0.5 0.5 [1..1000]

--      , bench "morris 10000"  . nfIO $ morris  0.5 0.5 [1..10000]
--      , bench "morris' 10000" . nfIO $ morris' 0.5 0.5 [1..10000]
--  ]


{-----------------------------------------------------------------------------
  Benchmark  t of m vs m of t
------------------------------------------------------------------------------}

---- * conclusion : counters faster by 1 ms on 0.05 0.05
--main :: IO ()
--main = defaultMain . return $ bgroup "t of m vs m of t tests" [
--        bench "eps = 0.5 d = 0.5 counter " . nfIO $ incrsIO $ counters  0.5 0.5
--      , bench "eps = 0.5 d = 0.5 counter'" . nfIO $ incrsIO $ counters' 0.5 0.5
   
--      , bench "eps = 0.1 d = 0.1 counter" . nfIO $ incrsIO $ counters   0.1 0.1
--      , bench "eps = 0.1 d = 0.1 counter'" . nfIO $ incrsIO $ counters' 0.1 0.1

--      , bench "eps = 0.05 d = 0.05 counter" . nfIO $ incrsIO $ counters   0.05 0.05
--      , bench "eps = 0.05 d = 0.05 counter'" . nfIO $ incrsIO $ counters' 0.05 0.05
--  ]


--incrsIO :: MonadRandom m => [[Counter]] -> m [[Counter]]
--incrsIO  xxs = runRVar (incrs  xxs) StdRandom

--counters :: Eps -> Delta  -> [[Counter]]
--counters e d = replicate m $ replicate t 0
--  where (m,t) = (m' e d, t' e d)

--counters' :: Eps -> Delta -> [[Counter]]
--counters' e d = replicate t $ replicate m 0
--  where (m,t) = (m' e d, t' e d)

--t' e d = round $ 1/(e^2*d)
--m' e d = round . log $ 1/d



---- * given a list of list of counters toss a coin for each counter and incr
--incrs :: [[Counter]] -> RVar [[Counter]]
--incrs = sequence . fmap (sequence . fmap incr)

---- * Increment a counter `x` with probability 1/2^x
--incr :: Counter -> RVar Counter
--incr x = do
--  h <- toss . coin $ 0.5^(round x)
--  return $ if isHead h then (seq () succ x) else seq () x




{-----------------------------------------------------------------------------
    Benchmark time compleity of mean/median
       see : http://www.serpentine.com/criterion/tutorial.html
------------------------------------------------------------------------------}

-- * observation: 5% difference in runtime using mean   vs not
-- *              5% difference in runtime using median vs not
--main :: IO ()
--main = defaultMain . return $ bgroup "morris mean vs no median" [
--      bench "morris median    100 items eps = 0.1   delta = 0.1 " . nfIO $ morris  0.1 0.1   [1..100]
--    , bench "morris no median 100 items eps = 0.1   delta = 0.1 " . nfIO $ morris' 0.1 0.1   [1..100]
--    , bench "morris    median 100 items eps = 0.05  delta = 0.05" . nfIO $ morris  0.05 0.05 [1..100]
--    , bench "morris no median 100 items eps = 0.05  delta = 0.05" . nfIO $ morris' 0.05 0.05 [1..100]
--  ]

{-----------------------------------------------------------------------------
    II. Approximate Mean
------------------------------------------------------------------------------}

-- * Run Morris alpha on stream inputs `xs`
--morrisA :: [a] -> IO Counter
--morrisA xs = flip runRVar StdRandom $ Cl.sourceList xs $$ alpha

---- * Run Morris beta on stream inputs `xs` for `t` independent trials and average
--morrisB :: Int -> [a] -> IO Counter
--morrisB t =  fmap rmean . replicateM t . morrisA

---- * final morris algorithm
---- * Run on stream inputs `xs` for t independent trials for `t = 1/eps`, 
---- * and `m` times in parralell, for `m = 1/(e^2 * d)`
---- * and take the median
---- * TODO: make this actually parralell
--morris :: Eps -> Delta -> [a] -> IO Counter
--morris e d = fmap rmedian . replicateM m . morrisB t 
--  where (t,m) = (round $ 1/(e^2*d), round $ 1/d)


------------------------------------------------------------------------------
--    III. Utils
-------------------------------------------------------------------------------

---- * Utils * -- 

---- * A step in morris Algorithm alpha
--alpha :: Sink a RVar Counter
--alpha = (\x -> 2^(round x) - 1) <$> Cl.foldM (\x _ -> incr x) 0


---- * Increment a counter `x` with probability 1/2^x
--incr :: Counter -> RVar Counter
--incr x = do
--  h <- toss . coin $ 0.5^(round x)
--  return $ if isHead h then (seq () succ x) else seq () x


--rmean, rmedian :: (Floating a, Ord a, RealFrac a) => [a] -> Float
--rmean   = fromIntegral . round . mean
--rmedian = fromIntegral . round . median



{-----------------------------------------------------------------------------
    Depricated
------------------------------------------------------------------------------}

-- * Non fmap f version to test list traversal
-- * marginal difference due to fmap g where g = mean or median
--morrisB' :: Int -> [a] -> IO [Counter]
--morrisB' t = replicateM t . morrisA

--morris' :: Eps -> Delta -> [a] -> IO [Counter]
--morris' e d = replicateM m . morrisB t 
  --where (t,m) = (round $ 1/(e^2*d), round $ 1/d)


{-----------------------------------------------------------------------------
  Benchmark  median of means
  naive "imperitive" solution worse in large n 
------------------------------------------------------------------------------}

--main :: IO ()
--main = defaultMain . return $ bgroup "count vs count'" [
--      --  bench "median  1000"  $ whnf (medianOfMeans  10) [1..1000]
--      --, bench "median' 1000"  $ whnf (medianOfMeans' 10) [1..1000]

--      --, bench "median 10000"  $ whnf (medianOfMeans  10) [1..10000]
--      --, bench "median' 10000"  $ whnf (medianOfMeans' 10) [1..10000]

--      --, bench "median 100000"  $ whnf (medianOfMeans  10) [1..100000]
--      --, bench "median' 100000"  $ whnf (medianOfMeans' 10) [1..100000]

--        bench "median  0.1"  $ whnf (medianOfMeans  1000000) [1..5000000]
--      , bench "median' 0.1"  $ whnf (medianOfMeans' 1000000) [1..5000000]

--      , bench "median  0.5"  $ whnf (medianOfMeans  8000) [1..24000]
--      , bench "median' 0.5"  $ whnf (medianOfMeans' 8000) [1..24000]

--  ]

---- * this is worse!!
--medianOfMeans' :: Counter -> [Counter] -> Counter
--medianOfMeans' t xs = let (ms,_,_) = foldr (tomean t) ([],0,1) xs in median ms

--tomean :: Counter -> Counter -> ([Counter],Counter,Counter) -> ([Counter],Counter,Counter)
--tomean t x (ms,m,c) | c < t      = (ms,m+x,c+1)
--                    | otherwise  = ((fromIntegral . round $ (m+x)/t):ms,0,1)


--medianOfMeans :: Int -> [Counter] -> Counter
--medianOfMeans t = median . fmap mean' . (chunksOf t) 
-- fmap median' . 

--ms = medianOfMeans 10 [1..100]
--(ms',_,_) = medianOfMeans' 10 [1..100]












