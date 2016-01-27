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



import System.IO.Unsafe


{-----------------------------------------------------------------------------
  Types 
------------------------------------------------------------------------------}

type W        = Int              -- * num cols
type D        = Int              -- * num rows
type P        = Int              -- * some prime   --> change this to Integer
type Event    = Int              -- * some input event from
                                 -- * universe: {1, .., i, .. n}

type Sketch   = Matrix Int       -- * D x W matrix  of counters
type ABs      = Matrix Int       -- * 2 x D matrix  of a_j and b_j for j = 1..d
type ColIdx   = [Int]            -- * indices of counters to increment
type Hash     = Event -> ColIdx  -- * Maps event to d x 1 list of column indices


-- * Algorithm State
type AState   = (Sketch,Hash,(D,W,P))


-- * the algorithm uses `some m` equipped with state and randomness
-- * also consider `some'` `a`
type Some m   = (MonadRandom m, MonadState AState m)

{-----------------------------------------------------------------------------
    Count Min Sketch 
------------------------------------------------------------------------------}

sk :: Batch Event IO AState
sk = sketch' ed 50

sketch' :: EpsDelta -> Event -> Batch Event IO AState
sketch' t n = sketch t n `using` eval


sketch :: Some m => EpsDelta -> Event -> Streaming Event m ()
sketch t n = lift (initDwp t n) $= inits False $= update

-- * there should be a combinator for this
update :: Some m => Streaming Event m ()
update = do
    mi <- await
    case mi of
        Nothing -> return ()
        Just i  -> lift (update' i) >> update


-- * there should be a combinator that does this
inits :: Some m => Bool -> Conduit Event m Event
inits False = lift inits' >> inits True
inits _     = pass

---- * a concrete `eval`uation of Some `m`
-- * run with vacuous inital condition to be thrown away
eval :: StateT AState (Rand StdGen) () -> IO AState
eval m = evalRandIO $ execStateT m $ (ones 1 1, const [], (0,0,0))


{-----------------------------------------------------------------------------
   Effectful Subroutines
   tenative conclusion, it works fine in here
--------------------------------------------------------------------------------}

initDwp :: Some m => EpsDelta -> Event -> m ()
initDwp (ED e d) n = setParam $ (round $ log2(1/d), round $ 2/e, round $ 2^n -1)

-- * initalize algorithm with zero counters and hash function
inits' :: Some m => m ()
inits' = do
    t       <- askParam
    hash    <- newHash t
    let sketch = newSketch t
    setHash   hash
    setSketch sketch

-- * on event `i`, update the `sketch`
update' :: Some m => Event -> m ()
update' i = do
    pm      <- askParam
    sketch  <- getSketch
    hash    <- askHash
    let incr = toMask (hash i) pm
    setSketch $ incr .+. sketch


-- * sketch1 == sketch1
-- * IO (sketch1', hash', t')
m1 = evalRandIO $ execStateT (update' i1) (sketch0, hash, tup)
m2 = evalRandIO $ execStateT (update' i2) (sketch1, hash, tup)

-- * Estimate
--estimate :: Some m => Event -> m ()
--estimate = do
--    ()


-- * intialize random cofficients a_j, b_j, for d x 2 matrix: 
-- * [ a_1 ... a_j ... a_d ]
-- * [ b_1 ... b_j ... b_d ]
newAbs :: MonadRandom m => (D,W,P) -> m ABs
newAbs (d,_,p) = (M.fromList 2 d . take (2*d)) <$> getRandomRs (1,p)


{-----------------------------------------------------------------------------
    Pure Subroutines
------------------------------------------------------------------------------}

newDwp :: EpsDelta -> Event -> (D,W,P)
newDwp (ED e d) n = (round $ log2(1/d), round $ 2/e, round $ 2^n -1)

-- * initalize the sketch
newSketch :: (D,W,P) -> Sketch
newSketch (d,w,_) = zeros d w

-- * h_j (i) = (a_j x i + b_j mod p) mod w
-- * sumRow-wise  [ a_1 ... a_j ... a_d ]  .*.  [ i_1 ...  i_d ]     mod p  mod w
-- *              [ b_1 ... b_j ... b_d ]       [ 1   ...  1   ]
-- * result:      [ idx_1, ..., idx_d   ]
-- *               where the indices start at 1
newHash :: MonadRandom m => (D,W,P) -> m Hash
newHash t = do
    ab  <- newAbs t
    return $ toHash' t ab

-- * Note `is` maps some `i` from event universe onto d x 2 matrix:
-- * [ i_1 ... i_2 ]
-- * [ 1   ...  1  ]
toHash' :: (D, W, P) -> ABs -> Hash
toHash' (d, w, p) ab i = go $ sumR (ab .*. is) ... (\v -> v `mod` p `mod` w)
    where go = fmap (+1) . toList
          is = (i .+ (zeros 1 d)) <-> ones 1 d

-- * given hash function, construct a binary mask of size `w` x `d`
-- * to increment sketch
toMask :: ColIdx -> (D,W,P) -> Sketch
toMask ks (d,w,p) = go w ks
    where
        go w ks = let vec c w = zeros 1 (c-1) <|> single 1 <|> zeros 1 (w - c) in
            case ks of
                c:[] -> vec c w
                c:cs -> vec c w <-> go w cs


{-----------------------------------------------------------------------------
    Ad hoc testing
------------------------------------------------------------------------------}

[i1,i2,i3] = [12,0,50]

tup@(d,w,p) = newDwp ed 50

hash      = unsafePerformIO . evalRandIO $ newHash tup
sketch0   = newSketch tup

-- * update it once
ks1       = hash i1
incr1     = toMask ks1 tup
sketch1   = incr1 .+. sketch0

-- * update again
ks2       = hash i2
incr2     = toMask ks2 tup
sketch2   = incr2 .+. sketch1



{-----------------------------------------------------------------------------
    Managing State
    TODO : redesign so this is not necessary
------------------------------------------------------------------------------}

askParam :: Some m => m (D,W,P)
askParam = get >>= \(_,_,p) -> return p

setParam :: Some m => (D,W,P) -> m ()
setParam t = get >>= \(s,h,_) -> put (s,h,t)

getSketch :: Some m => m Sketch
getSketch = get >>= \(s,_,_) -> return s

setSketch :: Some m => Sketch -> m ()
setSketch s = get >>= \(_,h,t) -> put (s,h,t)

askHash :: Some m => m Hash
askHash = get >>= \(_,h,_) -> return h

setHash :: Some m => Hash -> m ()
setHash h = get >>= \(s,_,t) -> put (s,h,t)

{-----------------------------------------------------------------------------
    Find a lib function for this:
------------------------------------------------------------------------------}

-- * identity conduit, why can't I find this in the library?
pass :: Monad m => Conduit i m i
pass  = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just x  -> yield x >> pass



