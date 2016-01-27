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
import Data.Conduit
import qualified Data.Conduit.List as Cl
import qualified Data.Matrix as M

import Core
import Utils
import Statistics


{-----------------------------------------------------------------------------
  Types 
------------------------------------------------------------------------------}

type W        = Int              -- * num cols
type D        = Int              -- * num rows
type P        = Int              -- * some prime   --> change this to Integer
type A        = Int              -- * coefficient a_j
type B        = Int              -- * coeeficient b_j
type Event    = Int              -- * some input event from
                                 -- * universe: {1, .., i, .. n}


type Sketch   = Matrix Int       -- * W x D matrix  of counters
type ABs      = Matrix Int       -- * 2 x D matrix  of a_j and b_j for j = 1..d
type Events   = Matrix Event     -- * 2 x D matrix of event i 
type Hash     = Event -> ColIdx  -- * Maps event to d x 1 list of index to incr
type ColIdx   = [Int]            -- * indices of counters to increment

type Some m   = (MonadRandom m, MonadState (Sketch, Hash) m, MonadReader (D,W,P) m)

{-----------------------------------------------------------------------------
    Count Min Sketch
------------------------------------------------------------------------------}



{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}

-- * initalize the sketch
sketch' :: D -> W -> Sketch
sketch' = zeros

-- * intialize random cofficients a_j, b_j, for d x 2 matrix: 
-- * [ a_1 ... a_j ... a_d ]
-- * [ b_1 ... b_j ... b_d ]
ab' :: MonadRandom m => P -> D -> m ABs
ab' p d = (M.fromList 2 d . take (2*d)) <$> getRandomRs (1,p)

-- * h_j (i) = (a_j x i + b_j mod p) mod w
-- * sumRow-wise  [ a_1 ... a_j ... a_d ]  .*.  [ i_1 ...  i_d ]     mod p  mod w
-- *              [ b_1 ... b_j ... b_d ]       [ 1   ...  1   ]
-- * result:      [ idx_1, ..., idx_d   ]
-- *               where the indices start at 1
hash' :: P -> W -> D -> ABs -> Hash
hash' p w d ab = \i ->  go $ sumR (ab .*. is) ... (\v -> v `mod` p `mod` w)
    where go = fmap (+1) . toList
          is = is' d i

-- * map `i` <- event univese onto d x 2 matrix of form
-- * [ i_1 ... i_2 ]
-- * [ 1   ...  1  ]
is' :: D -> Event -> Events
is' d i = (i .+ (zeros 1 d)) <-> ones 1 d


-- * given hash function, construct a binary mask of size `w` x `d`
-- * to increment sketch
mask' :: W -> ColIdx -> Sketch
mask' w ks = let vec c w = zeros 1 (c-1) <|> single 1 <|> zeros 1 (w - c) in
    case ks of
        c:[] -> vec c w
        c:cs -> vec c w <-> mask' w cs


-- * given input i and some hash function `h`, and sketch `ms`, 
-- * update `ms`
update' :: W -> D -> Hash -> Sketch -> Event -> Sketch
update' w d h ms i = mask' w (h i) .+. ms



maxi  = 30            :: Event
p     = 2^maxi - 1    :: P 
(d,w) = (4,3)         :: (D,W)
i     = 12            :: Event


mab    = evalRandIO $ ab' p d
sketch = sketch' d w
mhashs = (hash' p w d)  <$> mab

update h i = update' w d h sketch i

mhash  = head <$> mhashs



--hs = hashF p w xs ys

















