{-# LANGUAGE OverloadedStrings, FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification, GADTs, MultiParamTypeClasses, DeriveGeneric #-}

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Utils
-- | Creator: Xiao Ling
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------


module Utils (

    tick
  , log2

  , (.+.)
  , (.-.)
  , (.*.)
  , (./.)

  , (.+)
  , (.*)
  , (.^)

  , (...)
  , ones
  , zeros
  , single
  , size

  , sumC
  , sumR

  , foldC
  , foldR

  ) where
  

import Core
import Control.Monad
import Control.Monad.State
import Data.Matrix
import qualified Data.Matrix as M


{-----------------------------------------------------------------------------
  Monadic functions
------------------------------------------------------------------------------}

-- * `tick` up a counter
tick :: MonadState Counter m => m ()
tick = modify succ


{-----------------------------------------------------------------------------
  Basic Math
------------------------------------------------------------------------------}

log2 :: Floating a => a -> a
log2 = logBase 2 



{-----------------------------------------------------------------------------
  Matrix Operations
------------------------------------------------------------------------------}

-- * define precedance * -- 
infixl 6 .+.
infixl 6 .-.
infixl 7 ./.
infixl 7 .*.

infixr 8 .^
infixl 6 .+
infixl 7 .*

infixl 4 ...


-- * element wise operations between matrices * -- 

(.+.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.+.) = M.elementwise (+)

(.*.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.*.) = M.elementwise (*)

(.-.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.-.) = M.elementwise (-)

(./.) :: (Num a, Fractional a) => Matrix a -> Matrix a -> Matrix a
(./.) = M.elementwise (/)


-- * element wise operation from some num to matrix --

(.+) :: Num a => a -> Matrix a -> Matrix a
a .+ m = (+a) <$> m

(.*) :: Num a => a -> Matrix a -> Matrix a
a .* m = (*a) <$> m

(.^) :: (Num a, Integral a) => Matrix a -> a -> Matrix a
m .^ a = (^a) <$> m


-- * Folding row wise over matrix `m`, apply operation `g`
-- * element wise among rows
-- * ie: foldR (+) [ 1 1 1 ]       = [3 3 3]
-- *               [ 2 2 2 ]  
foldR :: (a -> a -> a) -> Matrix a -> Matrix a
foldR g m = go g m $ nrows m
  where go g m 1 = m
        go g m n = M.elementwise g (rowVector $ getRow n m) (go g m $ n - 1)

-- * Folding col wise 
-- * ie: foldC (+) [ 1 1 1 ]       =   [3
-- *               [ 2 2 2 ]            6]
foldC :: (a -> a -> a) -> Matrix a -> Matrix a
foldC g m = transpose $ foldR g (transpose m)


-- * Catamorphism row wise and column wise --

sumR :: Num a => Matrix a -> Matrix a
sumR = foldR (+)

sumC :: Num a => Matrix a -> Matrix a
sumC = foldC (+)


-- * common matrices * --


single :: Num a => a -> Matrix a
single a = a .+ zeros 1 1


ones :: Num a => Int -> Int -> Matrix a
ones m n = M.fromList m n $ replicate (m*n) 1

zeros :: Num a => Int -> Int -> Matrix a
zeros = M.zero

-- * syntactic sugar * -- 

(...) :: Num a => Matrix a -> (a -> b) -> Matrix b
(...) = flip fmap

size :: Num a => Matrix a -> (Int,Int)
size m = (nrows m, ncols m)


















