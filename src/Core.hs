{-# LANGUAGE OverloadedStrings, FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification, GADTs, MultiParamTypeClasses, DeriveGeneric #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Core
-- | Creator: Xiao Ling
-- | see    : https://hackage.haskell.org/package/QuickCheck-2.7.6/docs/Test-QuickCheck-Modifiers.html
-- |          https://hackage.haskell.org/package/random-1.1/docs/System-Random.html
-- |          https://github.com/athanclark/timemap/blob/master/src/Data/TimeMap.hs
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Core (

    Coin
  , Streaming
  , Batch

  , Prob
  , EpsDelta (ED)
  , Counter

  , ed

  , using
  , test

  , isHead
  , bias
  , coin
  , fair
  , toss
  , tosses

  ) where
  

import Data.Conduit
import qualified Data.Conduit.List as Cl

import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class
import Test.QuickCheck


{-----------------------------------------------------------------------------
  I. Data Types
------------------------------------------------------------------------------}

type Prob    = Float  
type Counter = Float

-- * A coin and its prob(Head)
data Coin = C { isHead :: Bool, bias :: Prob } 
  deriving Eq

-- * A streaming algorithm is some effectful computation with effect `m`, 
-- * input `a` and output `b`
type Streaming a m b  = Monad m => Sink a m b

-- * once a `StreamAlgo` is built, we may `test` it in `Batch` mode
-- * on some list of `a`s  with effectful computation `m`, 
-- * input `[a]` and output `b`
data Batch a m b      = Monad m => S { test :: [a] -> m b }

-- * epsilon and delta
data EpsDelta   = ED { eps :: Float, delt :: Float }
  deriving (Eq,Show,Ord)


{-----------------------------------------------------------------------------
  II. Type Class instances
------------------------------------------------------------------------------}

instance Show Coin where
  show (C h p) = show_ h ++ " Bias = " ++ show p 
    where show_ h | h         = "Head"
                  | otherwise = "Tail"

instance Arbitrary Coin where
  arbitrary      = coin <$> (choose (0,1) :: Gen Prob)
  shrink (C h p) = coin <$> shrink p 


instance Functor (Batch a b) where
  fmap g (S h) = S $ \xs -> g <$> h xs 

instance Monad m => Applicative (Batch a m) where
  pure  = return
  (<*>) = ap

-- * not sure if *there is* a right interpretation of `>>=`
-- * TODO: test monad properties
instance Monad m => Monad (Batch a m) where
  return       = S . const . return
  (S h) >>= g  = S $ \xs -> h xs >>= \a -> test (g a) xs


{-----------------------------------------------------------------------------
  III. Functions over (Eps,Delta)
------------------------------------------------------------------------------}

-- * "sufficiently accurate" is usually
-- * is within 0.05 of true value with 95% confidence
ed :: EpsDelta
ed = ED 0.05 0.05

{-----------------------------------------------------------------------------
  IV. Functions over TStreamAlgo
------------------------------------------------------------------------------}

-- * construct a test that runs a streaming algorithm `m` `using` eval`
using :: (Monad m2, Monad m1) => Streaming a m1 b -> (m1 b -> m2 c) -> Batch a m2 c
m `using` eval = S $ \xs -> eval $ Cl.sourceList xs $$ m


{-----------------------------------------------------------------------------
  V. Functions over Coin
------------------------------------------------------------------------------}

-- * Constructs a `Coin` with some bias `p`
-- * Wat: if not 0 <= p <=1, then we give you a fair coin by fiat
coin :: Prob -> Coin
coin p | 0 <= p && p <= 1 = C True p
       | otherwise        = fair

-- * Fair coin 
fair :: Coin
fair = coin 0.5

-- * `toss` a coin with bias `p`
-- * @Use: evalRandIO $ toss fair
toss :: MonadRandom m => Coin -> m Coin
toss c = face c <$> getRandomR (0,1 :: Prob)

-- * `toss` a coin `c` `n` times independently
-- * @Use: evalRandIO $ tosses 100 fair
tosses :: MonadRandom m => Int -> Coin -> m [Coin]
tosses n c = take n <$> face c `ffmap` getRandomRs (0,1 :: Prob)
  where ffmap = fmap . fmap

-- * Construct 
face :: Coin -> Prob -> Coin
face (C _ p) q = C (q <= p) p






























