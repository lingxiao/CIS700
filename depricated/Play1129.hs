{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, DeriveFunctor, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : First attempt at approximate mean
-- | Creator: Xiao Ling
-- | Created: 11/29/2015
-- | see    : https://github.com/snoyberg/conduit
--            https://gist.github.com/thoughtpolice/3704890
--            http://blog.ezyang.com/2012/01/problem-set-the-codensity-transformation/
--            http://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf
--            https://gist.github.com/supki/3776752
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Play1129 where


import System.Random

import Data.List
import Data.Random
import Data.Conduit
import qualified Data.Conduit.List as Cl

import Control.Monad.Identity
import Control.Monad.State

import qualified Test.QuickCheck as T

import Core
import Statistics




{-----------------------------------------------------------------------------
   1. make some examples
   2. make 
   3. see if cassava can be put into a conduit
   4. output Row type
   
   see : http://stackoverflow.com/questions/2376981/haskell-types-frustrating-a-simple-average-function
------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
    II. Approximate Median try ii
    TODO: figure out uniform sampling without knowing length of stream 
------------------------------------------------------------------------------}

-- * TODO: make this into an array since we'll be doing (!!) which is O(n) in list
-- * A reservoir is a list, its max allowed size, and its current size

-- * this deign is really bad, cannot gaurantee that currentSize is upToDate with length [a]
-- * dependent types would be good, or we can jsut get rid of current size
-- * or hide it in Core
data Reservoir a = R { run :: [a], size :: Int, currentSize :: Int }
  deriving (Show,Functor,Eq)


instance Num a => T.Arbitrary (Reservoir a) where

  -- * by construction, an arbitrary reservoir is always 1 item away from full
  arbitrary = do
    s <- T.arbitrary :: T.Gen (T.Positive Int)
    let s' = T.getPositive s
    let n = s' - 1 :: Int
    return $ R (replicate n 0) s' n

  shrink (R xs s n) = (\s' -> let n = max 0 (s' - 1) in (R (replicate n 0) s' n)) <$> T.shrink s



-- * constructor an empty resoivor of max size `s`
reservoir :: Int -> Reservoir a
reservoir s = R [] s 0

-- * check if reservoir is full
full :: Reservoir a -> Bool
full (R xs s n) = n >= s

-- * insert item, note the order
(>+) :: Reservoir a -> a -> Reservoir a
(>+) t@(R xs s n) x | full t     = t
                    | otherwise  = R (x:xs) s $ succ n

-- * remove the `i`th item from `r`
-- * if i > size then no item removed
(>-) :: Reservoir a -> Int -> Reservoir a
r@(R xs s n) >- i | i > n     = r
                  | otherwise = undefined      -- * really need to use array here


-- * sample an item `x` from the stream with uniform probability place in reservoir
-- * keep a conter `c` of elements seen
sampl :: Counter -> Sink a (StateT (Reservoir a) RVar) [a]
sampl i = do
  ma <- await
  case ma of 
    Nothing -> lift get >>= \r -> return $ run r
    Just a  -> do
      lift $ store i a
      sampl $ succ i


store :: Counter -> a -> StateT (Reservoir a) RVar ()
store i a = do
  r <- get
  case full r of
    False -> put $ r >+ a
    _     -> return ()
      where p = (read . show $ size r :: Float)/i



-- * test m/2 - e*m < rank(y) < m/2 + e*m



-- * quickCheck Resoivor

-- * full resoivor is full
prop_fullReso :: Reservoir Int -> T.Property
prop_fullReso r = (T.==>) (size r > 0) $ full (r>+1) == True


---- * inserting into non-full resoivor resutls in additional element
prop_insertNotFull :: Reservoir Int -> T.Property
prop_insertNotFull r = (T.==>) (size r > 0) $ currentSize (r>+1) == currentSize r + 1


-- * inserting into full resoivor leaves it unchanged
prop_insertFull :: Reservoir Int -> T.Property
prop_insertFull r = (T.==>) (size r > 0) $ currentSize (r'>+1) == currentSize r'
  where r' = r >+ 1


testReservoir :: IO ()
testReservoir = do
  T.quickCheck $ prop_fullReso
  T.quickCheck $ prop_insertFull
  T.quickCheck $ prop_insertNotFull


{-----------------------------------------------------------------------------
    II. Approximate Median Naive
------------------------------------------------------------------------------}

-- * setting problem: can you make this streaming? assuming you know m?
-- * can you do running uniform distribution?

-- * Find approximate eps e-median of list `xs` 
-- * Setting: list given before hand for some parameter t
-- * Use : runRVar (naive xs e d) StdRandom
medianNaive :: (Floating a, Ord a) => [a] -> Eps -> Delta -> RVar a
medianNaive xs e d | m <= t    = return $ median xs
             | otherwise = fmap median $ sampl' xs m t
              where
                m = length xs
                t = round $ 7/(e^2) * log (2/d)

-- * use vector here due to (!!) 
-- * sample `t` items from the list `[0..m-1]` *with replacement*
-- * Use : runRVar (sample m t) StdRandom
sampl' :: [a] -> Int -> Int -> RVar [a]
sampl' xs m t | m < 0 || t < 0   = return []
          | otherwise        = (\is -> [ xs !! i | i <- is]) <$> mis
            where mis = fmap sort . replicateM t $ uniform 0 (m-1) 

{-----------------------------------------------------------------------------
    III. Frequency Moments
------------------------------------------------------------------------------}






{-----------------------------------------------------------------------------
    I. Approximate Counting
------------------------------------------------------------------------------}


-- * Run on stream inputs `xs` for t independent trials for `t = 1/eps`, 
-- * and `m` times in parralell, for `m = 1/(e^2 * d)`
-- * and take the median
morris :: Eps -> Delta -> [a] -> RVar Counter
morris e d xs = fmap rmedian . (fmap . fmap) rmean        -- * take median of the means
              $ Cl.sourceList xs $$ count                 -- * stream to counter
              $ replicate m $ replicate t 0               -- * make m counters of length t
              where
                t  = round $ 1/(e^2*d)
                m  = round $ 1/d



-- * Given an m-long list `xs` of lists (each of which is t-lengthed) of counters,
-- * consume the stream and output result
count :: [[Counter]] -> Sink a RVar [[Counter]]
count xxs = (fmap . fmap . fmap) (\x -> 2^(round x) - 1) 
            $ Cl.foldM (\xs _ -> incrs xs) xxs

-- * given a list of list of counter `xs` of length `n`, 
-- * toss a coin and count
incrs :: [[Counter]] -> RVar [[Counter]]
incrs = sequence . fmap incr where
  incr xs = do
    hs <- sequence $ (\x -> toss . coin $ 0.5^(round x)) <$> xs 
    return $ pincr <$> zip hs xs  
      where pincr (h,x) = if isHead h then (seq () succ x) else seq () x



-- * Naive solution * -- 


-- * Run Morris alpha on stream inputs `xs`
morrisA :: [a] -> IO Counter
morrisA xs = flip runRVar StdRandom $ Cl.sourceList xs $$ alpha

-- * Run Morris beta on stream inputs `xs` for `t` independent trials and average
morrisB :: Int -> [a] -> IO Counter
morrisB t =  fmap rmean . replicateM t . morrisA

-- * Naive morris algorithm
-- * Run on stream inputs `xs` for t independent trials for `t = 1/eps`, 
-- * and `m` times in parralell, for `m = 1/(e^2 * d)`
-- * and take the median
-- * Problem : `replicateM n` is O(n) time
morris' :: Eps -> Delta -> [a] -> IO Counter
morris' e d = fmap rmedian . replicateM m . morrisB t 
  where (t,m) = (round $ 1/(e^2*d), round $ 1/d)


-- * A step in morris Algorithm alpha
alpha :: Sink a RVar Counter
alpha = (\x -> 2^(round x) - 1) <$> Cl.foldM (\x _ -> incr x) 0


-- * Increment a counter `x` with probability 1/2^x
incr :: Counter -> RVar Counter
incr x = do
  h <- toss . coin $ 0.5^(round x)
  return $ if isHead h then (seq () succ x) else seq () x


rmean, rmedian :: (Floating a, Ord a, RealFrac a) => [a] -> Float
rmean   = fromIntegral . round . mean
rmedian = fromIntegral . round . median


---- * Increment a counter `x` with probability 1/2^x
--incr' :: Counter -> RVar Counter
--incr' x = do
--  h <- (\q -> q <= (0.5^(round x) :: Prob)) <$> uniform 0 1
--  return $ if h then (seq () succ x) else seq () x


-- * Test Morris
tMorris :: IO ()
tMorris = undefined




{-----------------------------------------------------------------------------
   X. Finger Exercises

   type Source    m a = ConduitM () a   m ()   -- no meaningful input or return value
   type Conduit a m b = ConduitM a  b   m ()   -- no meaningful return value
   type Sink    a m b = ConduitM a Void m b    -- no meaningful output value
------------------------------------------------------------------------------}

-- * generator
genRs :: Monad m => Source m Int
genRs = Cl.sourceList rs

-- * conduits
cond :: Monad m => Conduit Int m String
cond = Cl.map show

add1 :: Monad m => Conduit Int m Int
add1 = Cl.map (+1)

add2 :: Monad m => Conduit Int m Int
add2 = do
   mx <- await
   case mx of
      Nothing -> return ()   
      Just x  -> (yield $ x + 2) >> add2

-- * sinks
sink1 :: Sink Int IO ()
sink1 = Cl.mapM_ $ putStrLn . show

sink2 :: Sink String IO ()
sink2 = Cl.mapM_ putStrLn

-- * accumlate the values in a list
sink3 :: Monad m => [Int] -> Sink Int m [Int]
sink3 xs = do 
   mx <- await
   case mx of
      Nothing -> return xs
      Just x  -> sink3 $ xs ++ [x]

-- * accuulate values in a list and increment by 3
add3 :: (Num a, Monad m) => Sink Int m [Int]
add3 = Cl.fold (\xs x -> xs ++ [x+3]) []

-- * some trivial pipes 
pp0, pp1, pp2 :: IO ()
pp0 = genRs $$ sink1
pp1 = genRs $$ add1 $= cond $= sink2
pp2 = genRs $$ add2 $= cond $= sink2


-- * map and fold
pp2' :: Identity [Int]
pp2' = genRs $$ add2 $= sink3 []

-- * fold a list
pp3 :: Identity [Int]
pp3 = genRs $$ add3

-- * counts everything
brute :: [a] -> Counter
brute xs = runIdentity $ Cl.sourceList xs $$ Cl.fold (\n _ -> succ n) 0 

-- * filter a list
pp4 :: Int -> Identity [Int]
pp4 n = genRs $= Cl.filter (>n) $$ Cl.fold (flip $ (++) . pure) []

-- * map list
pp5 :: Identity [Int]
pp5 = genRs $= Cl.map (+5) $$ sink3 []

-- * test property
tpp2, tpp3, tpp5 :: Bool
tpp2 = runIdentity pp2' == fmap (+2) rs
tpp3 = runIdentity pp3  == fmap (+3) rs
tpp5 = runIdentity pp5  == fmap (+5) rs

-- * test uniformness of list, observe it's relatively uniform
tpp4 :: [Int]
tpp4 = length . runIdentity . pp4 <$> [1..10]



{-----------------------------------------------------------------------------
    X. Test Data
------------------------------------------------------------------------------}


-- * random string of choice
rs :: (Enum a, Num a) => [a]
rs = [1..1000]

{-----------------------------------------------------------------------------
  Depricated
------------------------------------------------------------------------------}


--morrisA' :: Sink a RVar Counter
--morrisA' = Cl.foldM (\x _ -> incr x) 0

--incr :: MonadRandom m => Counter -> m Counter
--incr x = do
--  h <- toss . coin $ 0.5^x
--  return $ if isHead h then (seq () succ x) else seq () x


--morrisB :: Conduit a (StateT Counter RVar) a
--morrisB = do
--  mi <- await
--  case mi of
--    Nothing -> return ()
--    Just i  -> do
--      yield i
--      x <- lift get
--      h <- lift . lift . toss . coin $ 0.5^x
--      let x' = if isHead h then succ x else x
--      lift . put $ seq () x'
--      morrisB

--cap :: Monad m => Sink a m ()
--cap = do
--  mi <- await
--  case mi of
--    Nothing -> return ()
--    _       -> cap


--morrisBs = flip runStateT 0 $ Cl.sourceList [1..10000] $= morrisB $= morrisB $$ cap




-- * Morris Algorithm Beta, repeat step in morris alpha `t` times
--morrisB :: Trials -> Sink a RVar [Counter]
--morrisB t = replicateM t morrisA

--tmorrisB' :: Trials -> [a] -> IO [Counter]
--tmorrisB' t xs = (fmap . fmap) toN . flip runRVar StdRandom $ Cl.sourceList xs $$ morrisB t














