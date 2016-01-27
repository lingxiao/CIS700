{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, ConstraintKinds, ExistentialQuantification, GADTs, RankNTypes, MultiParamTypeClasses   #-}
{-# LANGUAGE ImpredicativeTypes, RankNTypes, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Count-Min Sketch
-- | Creator: Xiao Ling
-- | Created: 12/17/2015
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module CountMinSketch where

import Control.Monad.Random.Class
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Matrix
import Data.Foldable (toList)
import Data.Hashable
import Data.Conduit
import Data.Sequence (Seq,(|>),update,empty,index)
import qualified Data.Sequence as S
import qualified Data.Conduit.List as Cl

import Core
import Utils
import Statistics


{-----------------------------------------------------------------------------
  Types 
------------------------------------------------------------------------------}

-- * The table has width `W` and height `D`
-- * items in input stream comes from universe [n] = {1...n} 
-- * and let p = 2^n - 1
-- * hash function i |-> a * i + b mod p mod w
type W        = Int
type D        = Int
type P        = Int
type A        = Int
type B        = Int

-- * Our `Table` is a table of `Counters` of size W x D
type Counters = Seq Int
type Table    = (Counters, W, D)

-- * hash function `HashF` = a * i + b mod p mod w
-- * which maps some `i :: a` to `a`
-- * hash functions `HashFs` is a sequence of hash functions
type HashF    = Int -> Int
type HashFs   = Seq HashF

-- * Our algorithm modifies the `Table`, `HashF`unctions, a `Counter`,
-- * and is equipped with randomness
--type Some m = (MonadState (Table, HashFs) m, MonadState Counter m, MonadRandom m)

type Some m   = (MonadState (Table, HashFs) m, MonadRandom m)


{-----------------------------------------------------------------------------
  Play with the correct data type before jumping into it
------------------------------------------------------------------------------}

-- * declare random a for a_1 ... a_d
as :: MonadRandom m => P -> m [A]
as p = take p <$> getRandomRs (1,p)



{-----------------------------------------------------------------------------
  Subroutines
------------------------------------------------------------------------------}

-- * A vacuous inital condition to be thrown away
em :: (Table,HashFs)
em = (,) (S.replicate 0 0, 0, 0) empty


-- * initalize a table given `eps`ilon and `delt`a
table :: Some m => EpsDelta -> m Table
table (ED eps del) = return $ (S.replicate (w * d) 0, w, d)
    where (w,d) = (ceiling $ 2/eps, ceiling $ log2 (1/del))


-- * pick `a :: A` and `b ::B` of hash function given `p ::P` 
-- * that is we pick and b uniformly from [1..p]
ab :: Some m => P -> m (A,B)
ab p = (\(a:b:_) -> (a,b)) <$> take 2 <$> getRandomRs (1,p)

-- * construct a `hashF`unction given `p` and `w` 
hashF :: Some m => P -> W -> m HashF
hashF p w = do
    (a,b) <- ab p
    return $ \i -> ((a * i + b) `mod` p) `mod` w


hs :: Some m => m [(A,B)]
hs = replicateM 10 $ ab 100


-- * construct `D` pairwise independent `hashF`unctions 
-- * TODO : make sure they're actually different
hashFs :: Some m => D -> P -> W -> m HashFs
hashFs d p = S.replicateM d . hashF p 


-- * initalize count min algorithm
inits :: Some m => EpsDelta -> P -> m ()
inits eps p = do
    t@(_,w,d) <- table eps
    hs        <- hashFs d p w
    put (t,hs)


update :: Some m => m ()
update = do
    ((cs,w,d), hs) <- get
    return ()












{-----------------------------------------------------------------------------
  CountMinSketch
------------------------------------------------------------------------------}










