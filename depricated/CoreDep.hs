{-# LANGUAGE OverloadedStrings, FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes, MultiParamTypeClasses, DeriveGeneric #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Core
-- | Creator: Xiao Ling
-- | see    : https://hackage.haskell.org/package/QuickCheck-2.7.6/docs/Test-QuickCheck-Modifiers.html
-- |          https://hackage.haskell.org/package/random-1.1/docs/System-Random.html
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------


module Core (

    Prob
  , Counter
  , Delta
  , Eps
  , Coin

  , isHead
  , bias

  , coin
  , fair

  , toss
  , tosses

  ) where
  

import Control.Monad
import Test.QuickCheck
import Data.Random


{-----------------------------------------------------------------------------
  I. Data Types
------------------------------------------------------------------------------}

type Counter = Float
type Delta   = Double
type Eps     = Double
type Prob    = Double  


-- * A coin and its prob(Head)
data Coin = C { isHead :: Bool, bias :: Prob } 
  deriving Eq

{-----------------------------------------------------------------------------
  II. Type Class
------------------------------------------------------------------------------}

instance Show Coin where
  show (C h p) = show_ h ++ " Bias = " ++ show p 
    where show_ h | h         = "Head"
                  | otherwise = "Tail"

instance Arbitrary Coin where
  arbitrary      = coin <$> (choose (0,1) :: Gen Prob)
  shrink (C h p) = coin <$> shrink p 


{-----------------------------------------------------------------------------
  III. Functions over Coin
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
-- * Use: runRVar (toss fair) StdRandom
toss :: Coin -> RVar Coin
toss (C _ p) = (\q -> C (q <= p) p) <$> uniform 0 1

-- * `toss` a coin `c` `n` times independently
-- * Use : runRVar (tosses fair) StdRandom
tosses :: Coin -> Int -> RVar [Coin]
tosses _ 0 = return []
tosses c n = toss c `cons` (tosses c $ pred n)
  where cons = liftM2 (:)


{-----------------------------------------------------------------------------
  IV. Basic Notions
------------------------------------------------------------------------------}


































