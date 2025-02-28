module Tier2.Writer (collectAndSumInOrder) where

import Control.Monad.Writer
import Tier0.Writer (Tree (..))

collectAndSumInOrder :: Num a => Tree a -> Writer (Sum a) [a]
collectAndSumInOrder (Leaf v) = do
				tell (return v)
				return [v]
collectAndSumInOrder (Branch leftBranch v rightBranch) = do
							lsum <- collectAndSumInOrder leftBranch
							tell (return v)
							rsum <- collectAndSumInOrder rightBranch
							return (lsum ++ [v] ++ rsum) 
