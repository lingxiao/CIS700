{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, ConstraintKinds, ExistentialQuantification, GADTs, RankNTypes, MultiParamTypeClasses, RankNTypes, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Approximate median
-- | Creator: Xiao Ling
-- | Created: 12/08/2015
-- | TODO   : test standard deviation of alpha, beta, and final version
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ApproxMedian (

      MaxSize
    , Store

    , median
    , median'

  ) where

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Random.Class

import Data.Conduit
import Data.Foldable (toList)
import qualified Data.Sequence as S
import qualified Data.Conduit.List as Cl
import Data.Sequence (Seq,(|>),update,empty,index)

import Test.QuickCheck
import Test.QuickCheck.Random

import Core
import Utils
import qualified Statistics as St


{-----------------------------------------------------------------------------
  Types 
------------------------------------------------------------------------------}

type MaxSize = Float
type Store a = (Seq a, MaxSize)


-- * the algorithm uses `some m` equipped with state and randomness
-- * also consider `some'` `a`
type Some  m = (MonadState Counter m, MonadRandom m)
type Some' a = (Floating a          , Ord a        )


{-----------------------------------------------------------------------------
  Approximate Median 
------------------------------------------------------------------------------}

-- * @Use: test (tMedian (E 0.05, D 0.05)) xs
-- * compute `tMedian` within accuracy `Eps` and confidence `Delta`
median' :: Some' a => EpsDelta -> Batch a IO a
median' t = median t `using` eval

-- * `tick` up a counter for each item `a` seen and with probability 
-- * min(1, s/i) put item `a` into store `t` of maxSize `s`
median :: (Some m, Some' a) => EpsDelta -> Streaming a m (Store a)
median (ED e d) = Cl.foldM step $ store s
  where s = 7/(e^2) * log (2/d)

-- * a concrete `eval`uation of `m`
eval :: Some' a => StateT Counter (Rand StdGen) (Store a) -> IO a
eval m = fmap (St.median . toList) . fmap fst . evalRandIO $ evalStateT m 0

{-----------------------------------------------------------------------------
  Subroutines. Read $|> to understand the actual algorithm
------------------------------------------------------------------------------}

-- * One step of the algorithm
step :: (Some m, Some' a) => Store a -> a -> m (Store a)
step t a = tick >> t $|> a

-- * with probability `s/i` uniformly select an item from the store `t` and 
-- * replace it with `a`.
-- * mnemonic: `|>` is insertion, `$f` means do f probabilistically
infixl 7 $|>
($|>) :: Some m => Store a -> a -> m (Store a)
($|>) t@(_,s) a = do
  i <- get
  if s > i then t $> a else do
    h <- toss . coin $ s/i
    if isHead h then t $> a else return t


{-----------------------------------------------------------------------------
  Utils
------------------------------------------------------------------------------}

-- * Construct an empty store given max size `s`
store :: MaxSize -> Store a
store = (,) empty

-- * Probabilistic insertion. If the store is not full, insert item `a` deterministically
-- * else with uniform probability replace some existing item in store with `a`
-- * mnemonic: `|>` is insertion, `$f` means do f probabilistically
($>) :: Some m => Store a -> a -> m (Store a)
(as,s) $> a | length as < round s = return (as |> a, s)
            | otherwise           = (\x -> (update (round x) a as, s)) 
                                  <$> getRandomR (0,s :: MaxSize)













