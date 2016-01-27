{-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Codensity tutorial
-- | Creator: Xiao Ling
-- | Created: 12/8/2015
-- | see    : Asymptotic Improvement of Computations over Free Monads by Janis Voigtlader
-- |          http://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf
-- |
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

import Control.Monad


{-----------------------------------------------------------------------------
    Data Type and Type Class
------------------------------------------------------------------------------}

data Tree a = Leaf a | Tree a `Node` Tree a 
    deriving (Eq,Show)


instance Functor Tree where
    fmap g (Leaf a)     = Leaf $ g a
    fmap g (Node t1 t2) = Node (fmap g t1) (fmap g t2)

instance Applicative Tree where
    pure  = return
    (<*>) = ap

instance Monad Tree where
    return             = Leaf
    (Leaf a) >>= g     = g a
    (Node t1 t2) >>= g = Node (t1 >>= g) (t2 >>= g)


{-----------------------------------------------------------------------------
    Functions over Tree
------------------------------------------------------------------------------}

-- * in order traversal by fiat
toList :: Tree a -> [a]
toList (Leaf a)     = [a]
toList (Node t1 t2) = toList t1 ++ toList t2

-- * not quite the same ... 
fullTree :: Int -> Tree Int
fullTree 1 = Leaf 1
fullTree n = do
    i <- fullTree $ n - 1
    Node (Leaf $ n - 1) (Leaf $ n - 2)


{-----------------------------------------------------------------------------
    Tree Examples
------------------------------------------------------------------------------}

ls :: [Tree Int]
ls@[l1,l2,l3,l4,l5,l6] = Leaf <$> [1..6]

t1 :: Tree Int
t1 = Node (Node l1 l2) (Node l3 l4)

{-----------------------------------------------------------------------------
    Datatype - abstract over leaves of a tree
------------------------------------------------------------------------------}
























