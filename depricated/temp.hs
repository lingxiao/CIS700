{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -XBangPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, TypeSynonymInstances #-}





import Data.Random
import Data.Conduit
import Data.List
import Data.Ord (comparing)
import qualified Data.Conduit.List as Cl

import Control.Monad.Identity


{-----------------------------------------------------------------------------
   1. make some examples
   2. make 
   3. see if cassava can be put into a conduit
   4. output Row type
   
   see : http://stackoverflow.com/questions/2376981/haskell-types-frustrating-a-simple-average-function
------------------------------------------------------------------------------}


type Prob    = Double
type Counter = Float
type Delta   = Double
type Eps     = Double


{-----------------------------------------------------------------------------
    SO question
------------------------------------------------------------------------------}


-- * Run Morris alpha on stream inputs `xs`
morrisA :: [a] -> IO Counter
morrisA xs = flip runRVar StdRandom $ Cl.sourceList xs $$ alpha

-- * Run Morris beta on stream inputs `xs` for `t` independent trials and average
morrisB :: Int -> [a] -> IO Counter
morrisB t =  fmap rmean . replicateM t . morrisA

-- * final morris algorithm
-- * Run on stream inputs `xs` for t independent trials for `t = 1/eps`, 
-- * and `m` times in parralell, for `m = 1/(e^2 * d)`
-- * and take the median
-- * TODO: make this actually parralell
morris :: Eps -> Delta -> [a] -> IO Counter
morris e d = fmap rmedian . replicateM m . morrisB t 
  where (t,m) = (round $ 1/(e^2*d), round $ 1/d)


-- * Utils * -- 

-- * A step in morris Algorithm alpha
alpha :: Sink a RVar Counter
alpha = (\x -> 2^(round x) - 1) <$> Cl.foldM (\x _ -> incr x) 0

-- * Increment a counter `x` with probability 1/2^x
incr :: Counter -> RVar Counter
incr x = do
  h <- (\q -> q <= (0.5^(round x) :: Prob)) <$> uniform 0 1
  return $ if h then (seq () succ x) else seq () x

rmean, rmedian :: (Floating a, Ord a, RealFrac a) => [a] -> Float
rmean   = fromIntegral . round . mean
rmedian = fromIntegral . round . median

-- |Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- |Median
median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x


{-----------------------------------------------------------------------------
    III. Redux version for SO
------------------------------------------------------------------------------}


--toss :: Double -> RVar Bool
--toss p = do
--  q <- uniform 0 1
--  return $ q <= p

--toss' :: MonadRandom m => Double -> m Bool
--toss' p = runRVar (toss p) StdRandom




--foo :: StateT Int RVar ()
--foo = do
--    h <- lift $ toss 0.5
--    if h then put 404 else put 200


----bar :: StateT Int RVar ()
--bar :: (MonadRandom m) => StateT Int m ()
--bar = do
--    h <- lift $ toss' 0.5
--    if h then put 100 else put 0



--testFoo :: MonadRandom m => m ((), Int)
--testFoo = runRVar (runStateT foo 0) StdRandom

--testBar :: MonadRandom m => m ((), Int)
--testBar = runRVar (runStateT bar 0) StdRandom





{-----------------------------------------------------------------------------
    III. Redux version for SO
------------------------------------------------------------------------------}

--tos' :: MonadRandom m => Prob -> m Bool
--tos' p = runRVar (tos p) StdRandom

--tos :: Prob -> RVar Bool
--tos p = do
--  q <- uniform 0 1
--  return $ q <= p

--toses :: Prob -> Int -> RVar [Bool]
--toses p 0 = return mempty
--toses p n = tos p `cons` (toses p $ pred n)
--  where cons = liftM2 (:)


--morris :: [a] -> RVar Int
--morris xs = go xs 0 where
--  go []     n = return n
--  go (_:xs) n = do
--    h <- tos $ 0.5^n
--    go xs $ if h then succ n else n


--morrisTest :: [a] -> IO Int
--morrisTest xs = do
--  runRVar (morris xs) StdRandom


--tos' :: Prob -> StdGen -> (Bool,StdGen)
--tos' p s = (q <= p, s')
--  where (q,s') = randomR (0,1) s

--tos :: Prob -> StdGen -> (Bool,StdGen)
--tos p s = (q <= 100*p, s')
--  where (q,s') = randomR (0,100) s

--toses :: Prob -> Tosses -> StdGen -> [(Bool,StdGen)]
--toses _ 0 _ = []
--toses p n s = let t@(_,s') = tos p s in t : toses p (n-1) s'

--toses' :: Prob -> Tosses -> StdGen -> [Bool]
--toses' p n = fmap fst . toses p n

--morris :: StdGen -> [a] -> Int
--morris s xs = go s xs 0 where
--  go _ []     n = n
--  go s (_:xs) n = go s' xs n' where
--    (h,s') = tos (0.5^n) s 
--    n'     = if h then (n + 1) else n