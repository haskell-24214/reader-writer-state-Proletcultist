module Tier0.Writer (Tree (..), sumAndTraceInOrder) where

import Control.Monad.Writer

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving Eq

sumAndTraceInOrder :: Num a => Tree a -> Writer [a] a
sumAndTraceInOrder (Leaf v) = do
				tell [v]
				return v
sumAndTraceInOrder (Branch leftBranch v rightBranch) = do
							lsum <- sumAndTraceInOrder leftBranch
							tell [v]
							rsum <- sumAndTraceInOrder rightBranch
							return (lsum + v + rsum) 
