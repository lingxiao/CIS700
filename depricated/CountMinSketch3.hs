{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, ConstraintKinds, ExistentialQuantification, GADTs, RankNTypes, MultiParamTypeClasses   #-}
{-# LANGUAGE ImpredicativeTypes, RankNTypes, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Count-Min Sketch       original 
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
import Data.Conduit.Internal (idP)
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
type Event    = Int              -- * some input event from
                                 -- * universe: {1, .., i, .. n}

type Param    = (EpsDelta,Event) -- * Count Min Sketch is parameterized by
                                 -- * desired eps and delta, and maximum event value                             

type Sketch   = Matrix Int       -- * D x W matrix  of counters
type ABs      = Matrix Int       -- * 2 x D matrix  of a_j and b_j for j = 1..d
type ColIdx   = [Int]            -- * indices of counters to increment
type Hash     = Event -> ColIdx  -- * Maps event to d x 1 list of column indices

type Some m   = (MonadRandom m, MonadState (Sketch, Hash) m, MonadReader (D,W,P) m)

{-----------------------------------------------------------------------------
    Count Min Sketch 
------------------------------------------------------------------------------}

sketch' :: Batch Event IO (Sketch,Hash)
sketch' = sketch `using` eval


sketch :: Some m => Streaming Event m ()
sketch = inits False $= update


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

-- * a concrete `eval`uation of Some `m`
-- * run with vacuous inital condition `em` to be thrown away
eval :: StateT (Sketch,Hash) (ReaderT (D,W,P) (Rand StdGen)) () -> IO (Sketch,Hash)
eval m = evalRandIO $ runReaderT (execStateT m em) (4,3,2^30 - 1)
    where em = (zeros 0 0, const [])


{-----------------------------------------------------------------------------
    Count Min Sketch Steps
------------------------------------------------------------------------------}

-- * initalize algorithm with zero counters and hash function
inits' :: Some m => m ()
inits' = do
    sketch  <- newSketch
    hash    <- newHash
    put (sketch,hash)

-- * on event `i`, update the `sketch`
update' :: Some m => Event -> m ()
update' i = do
    (d,w,_)       <- ask
    (sketch,hash) <- get
    incr          <- toMask $ hash i 
    put (incr .+. sketch, hash)

-- * Estimate
--estimate :: Some m => Event -> m ()
--estimate = do
--    ()


{-----------------------------------------------------------------------------
    Subroutines
------------------------------------------------------------------------------}

-- * initalize the sketch
newSketch :: Some m => m Sketch
newSketch = do
    (d,w,_) <- ask
    return $ zeros d w

-- * h_j (i) = (a_j x i + b_j mod p) mod w
-- * sumRow-wise  [ a_1 ... a_j ... a_d ]  .*.  [ i_1 ...  i_d ]     mod p  mod w
-- *              [ b_1 ... b_j ... b_d ]       [ 1   ...  1   ]
-- * result:      [ idx_1, ..., idx_d   ]
-- *               where the indices start at 1
newHash :: Some m => m Hash
newHash = do
    (d,w,p) <- ask
    ab      <- newAbs
    return $ toHash' d w p ab

-- * Note `is` maps some `i` from event univese onto d x 2 matrix:
-- * [ i_1 ... i_2 ]
-- * [ 1   ...  1  ]
toHash' :: D -> W -> P -> ABs -> Hash
toHash' d w p ab i = go $ sumR (ab .*. is) ... (\v -> v `mod` p `mod` w)
    where go = fmap (+1) . toList
          is = (i .+ (zeros 1 d)) <-> ones 1 d

-- * intialize random cofficients a_j, b_j, for d x 2 matrix: 
-- * [ a_1 ... a_j ... a_d ]
-- * [ b_1 ... b_j ... b_d ]
newAbs :: Some m => m ABs
newAbs = do
    (d,_,p) <- ask
    (M.fromList 2 d . take (2*d)) <$> getRandomRs (1,p)


-- * given hash function, construct a binary mask of size `w` x `d`
-- * to increment sketch
toMask :: Some m => ColIdx -> m Sketch
toMask ks = ask >>= (\(_,w,_) -> return $ go w ks)
    where
        go w ks = let vec c w = zeros 1 (c-1) <|> single 1 <|> zeros 1 (w - c) in
            case ks of
                c:[] -> vec c w
                c:cs -> vec c w <-> go w cs


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




