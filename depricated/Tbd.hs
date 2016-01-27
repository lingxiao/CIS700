{-# LANGUAGE UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Random.Class



-- * fair coin
fair :: MonadRandom m => m Bool
fair = (flip (<=) 0.5) <$> getRandomR (0,1 :: Double)

-- * how do i run this 
bar :: (MonadState Int m, MonadRandom m) => m Bool
bar = fair >>= (\h -> put 404 >> return h)


bar' :: (Monad m, MonadRandom (StateT Int m)) => m Bool
bar' = evalStateT bar 0

-- evalRandIO = getStdRandom  . runRand
bar'' :: IO Bool
bar'' = evalRandIO bar'

fairs :: MonadRandom m => Int -> m [Bool]
fairs n = (take n) <$> (flip (<=) 0.5) `ffmap` getRandomRs (0,1 :: Double)
    where ffmap = fmap . fmap













--instance MonadState s m => MonadState s (RVarT m) where
--  get   = lift get
--  put   = lift . put
--  state = lift . state


--foo :: (MonadState s m, MonadRandom m) => s -> RVarT m ()
--foo s = do 
--  x <- (uniformT 0 1 :: RVarT m Double)
--  if x < 0.5 then put s else return ()


--foo' :: (MonadState String m, MonadRandom m) => RVarT m ()
--foo' = foo "hello"

--foo'' :: (MonadState String m, MonadRandom m) => m ()
--foo'' = undefined
----foo''= runRVarT foo' StdRandom 




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


cat :: (MonadState String m, MonadReader String m) => m String
cat = do
    x <- get
    y <- ask
    return $ x ++ y


cat1 :: Identity String
cat1 = evalStateT (runReaderT cat "world") "hello"


cat2 :: Identity String
cat2 = runReaderT (evalStateT cat "hello") "world"



bar :: (MonadTrans t1, MonadState t m, MonadState s (t1 m)) => s -> RVarT (t1 m) ()
bar s = do 
  x <- (uniformT 0 1 :: RVarT m Double)
  a <- lift . lift $ get
  if x < 0.5 then put s else return ()



