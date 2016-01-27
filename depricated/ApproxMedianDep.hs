{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


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

    , aMedian
    , aMedian'

  ) where


import Control.Monad.Trans 
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Random
import Data.Conduit
import Data.Sequence 
import Data.Foldable (toList)
import qualified Data.Conduit.List as Cl


import Core
import Statistics


{-----------------------------------------------------------------------------
  Types 
------------------------------------------------------------------------------}

type MaxSize = Int
type Store a = (Seq a, MaxSize)

-- * Construct an empty store given max size `s`
store :: MaxSize -> Store a
store = (,) empty

{-----------------------------------------------------------------------------
  Median II
------------------------------------------------------------------------------}

-- * determine the `e`ps -approx median of a list `xs` with confidence `d`elta.
aMedian :: (Ord a, Floating a) => Eps -> Delta -> [a] -> IO a
aMedian e d xs = eval $ Cl.sourceList xs $$ toMedian $ store s
  where eval p = fmap (median . toList) . fmap fst . flip runRVar StdRandom $ evalStateT p 0
        s      = round $ 7/(e^2) * log (2/d)

-- * `tick` up a counter for each item `a` seen
-- * and with probability min(1, s/i) put item `a` into store `t`
toMedian :: Store a -> Sink a (StateT Counter RVar) (Store a)
toMedian t = Cl.foldM (\t a -> tick >> t $|> a) t

-- * count items seen so far
tick :: Enum s => MonadState s m => m ()
tick = modify succ


-- * with probability `s/i` uniformly select an item from the store `t` and replace it with `a`
-- * mnemonic: `|>` is insertion for Sequences, and `$f` means do f probabilistically
($|>) :: Store a -> a -> StateT Counter RVar (Store a)
($|>) t@(as,s) a = do

  i <- get

  let p = s ./ i
  if p >= 1 then return (as |> a, s) else do

      h <- lift . toss $ coin p

      if isHead h then do
          x <- lift (uniform 0 s :: RVar Int)
          let as' = update x a as 
          return (as',s)
      else return t


-- * Sledghammer division 
(./) :: (Fractional a, Read a, Show s, Show i) => s -> i -> a
(./) s i = (read . show $ s)/(read . show $ i)



{-----------------------------------------------------------------------------
  Median II
------------------------------------------------------------------------------}

-- * determine the `e`ps - approx median of a list `xs` with confidence `d`elta.
aMedian' :: (Ord a, Floating a) => Eps -> Delta -> [a] -> IO a
aMedian' e d xs = eval $ Cl.sourceList xs $$ toMedian'
  where eval p = fmap (median . toList) . fmap fst $ runRVar (evalStateT (runStateT p 0) (store s)) StdRandom
        s      = round $ 7/(e^2) * log (2/d)


-- * `tick` up a counter `i` of items seen so far
-- * and with uniform probability put the item `a` into a store `as` of max size `s`
toMedian' :: Sink a (StateT Counter (StateT (Store a) RVar)) (Seq a)
toMedian' = tick' >> do
  ma <- await
  case ma of
    Nothing -> (lift . lift $ get) >>= return . fst
    Just a  -> (lift . place $ a ) >>  toMedian'


tick' :: (Enum s, MonadTrans t, MonadState s m) => t m ()
tick' = lift . modify $ succ        

-- * with uniform probability `s/i` select an item from the
-- * store and re`place` it with `a`
place :: a -> StateT Counter (StateT (Store a) RVar) ()
place a = do

  i        <- get
  t@(as,s) <- lift get

  let p = s ./ i

  if p >= 1 then lift $ put (as |> a, s) else do

      h <- lift . lift . toss $ coin p
      case isHead h of
        False -> lift $ put (as,s)
        _     -> do
          x <- lift $ lift (uniform 0 s :: RVar Int)
          let as' = update x a as
          lift $ put (as',s)


{-----------------------------------------------------------------------------
  Awaiting SO response for this stuff
------------------------------------------------------------------------------}

instance MonadState s m => MonadState s (RVarT m) where
  get   = lift get
  put   = lift . put
  state = lift . state


foo :: (MonadState s m, MonadRandom m) => s -> RVarT m ()
foo s = do 
  x <- (uniformT 0 1 :: RVarT m Double)
  if x < 0.5 then put s else return ()


foo' :: (MonadState String m, MonadRandom m) => RVarT m ()
foo' = foo "hello"

--foo'' :: (MonadState String m, MonadRandom m) => m ()
--foo''= runRVarT foo' StdRandom 


bar :: (MonadTrans t1, MonadState t m, MonadState s (t1 m)) => s -> RVarT (t1 m) ()
bar s = do 
  x <- (uniformT 0 1 :: RVarT m Double)
  a <- lift . lift $ get
  if x < 0.5 then put s else return ()


-- * desired behavior with state and ranndomness

baz :: (MonadState s m, MonadReader t m) => m ()
baz = get >> ask >> return ()

baz1 :: MonadReader t m => m ((),Int)
baz1 = runStateT baz 0 

baz1' :: Identity ((),Int)
baz1' = runReaderT baz1 (1,"hello")

baz2 :: MonadState s m => m ()
baz2 = runReaderT baz "hello"

baz2' :: Identity ((),[Int])
baz2' = runStateT baz2 [1..10]



















