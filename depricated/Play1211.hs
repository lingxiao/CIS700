{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, DeriveFunctor, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : First attempt at approximate mean
-- | Creator: Xiao Ling
-- | Created: 11/29/2015
-- | see    : https://github.com/snoyberg/conduit
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Play1129 where


import System.Random

import Data.List
import Data.Random
import Data.Conduit
import qualified Data.Conduit.List as Cl


import Data.Vector (Vector,fromList,cons,snoc,(!?))
import qualified Data.Vector       as V

import Control.Monad.Identity
import Control.Monad.State

import Test.QuickCheck ((==>), (.&&.))
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

-- * first figure out how to sample from a list with probability p
-- * it's a dice with 1/p faces 

--type MaxSize      = Int
--type Store a      = (Vector a, MaxSize)


--toMedian :: Counter -> Sink a (StateT (Store a) RVar) (Vector a)
--toMedian c = do
--  mi <- await
--  case mi of
--    Nothing -> lift get >>= return . fst
--    Just i  -> do
--      (v,s) <- lift get
--      case V.length v < s of
--        True -> do
--          let v' = i `cons` v
--          put 





{-----------------------------------------------------------------------------
    II. Approximate Median try ii
        too much unnnecessary stuff that adds no value
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
    let n = s' - 1   :: Int
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
sampl' :: Counter -> Sink a (StateT (Reservoir a) RVar) [a]
sampl' i = do
  ma <- await
  case ma of 
    Nothing -> lift get >>= \r -> return $ run r
    Just a  -> do
      lift $ store' i a
      sampl' $ succ i


store' :: Counter -> a -> StateT (Reservoir a) RVar ()
store' i a = do
  r <- get
  case full r of
    False -> put $ r >+ a
    _     -> return ()
      where p = (read . show $ size r :: Float)/i



-- * test m/2 - e*m < rank(y) < m/2 + e*m



-- * quickCheck Resoivor

-- * full resoivor is full
prop_fullRes :: Reservoir Int -> T.Property
prop_fullRes r = size r > 0 ==> full (r>+1) == True


---- * inserting into non-full resoivor resutls in additional element
prop_insertNotFull :: Reservoir Int -> T.Property
prop_insertNotFull r = size r > 0 ==> currentSize (r>+1) == currentSize r + 1


-- * inserting into full resoivor leaves it unchanged
prop_insertFull :: Reservoir Int -> T.Property
prop_insertFull r = size r > 0 ==> currentSize (r'>+1) == currentSize r'
  where r' = r >+ 1

props :: T.Property
props =  prop_fullRes .&&. prop_insertFull .&&. prop_insertFull

testReservoir :: IO ()
testReservoir = T.quickCheck props


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
             | otherwise = fmap median $ sampl'' xs m t
              where
                m = length xs
                t = round $ 7/(e^2) * log (2/d)

-- * use vector here due to (!!) 
-- * sample `t` items from the list `[0..m-1]` *with replacement*
-- * Use : runRVar (sample m t) StdRandom
sampl'' :: [a] -> Int -> Int -> RVar [a]
sampl'' xs m t | m < 0 || t < 0   = return []
          | otherwise        = (\is -> [ xs !! i | i <- is]) <$> mis
            where mis = fmap sort . replicateM t $ uniform 0 (m-1) 
