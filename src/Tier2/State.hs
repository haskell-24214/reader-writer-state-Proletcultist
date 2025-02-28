module Tier2.State where

import Control.Monad.State

data Registers = Registers { ax :: Int, bx :: Int, blink :: Bool, acc :: Int }

emptyRegisters = Registers 0 0 False 0

type Calculation = State Registers Int

plus :: Calculation
plus = do
	(Registers ax bx _ _) <- get
	put (Registers ax bx False (ax+bx))
	return (ax + bx)

minus :: Calculation
minus = do
	(Registers ax bx _ _) <- get
	put (Registers ax bx False (ax-bx))
	return (ax - bx)

productS :: Calculation
productS = do
	(Registers ax bx _ _) <- get
	put (Registers ax bx False (ax*bx))
	return (ax * bx)

divS :: Calculation
divS = do
	(Registers ax bx _ _) <- get
	put (if (bx /= 0) then (Registers ax bx False (ax `div` bx)) else (Registers 0 0 False 0))
	return (if (bx/=0) then (ax `div` bx) else 0) 

swap :: Calculation
swap = do
	(Registers ax bx blink acc) <- get
	put (Registers bx ax blink acc)
	return acc

blinkS :: Calculation
blinkS = do
	(Registers ax bx blink acc) <- get
	put (Registers ax bx (if blink then False else True) acc)
	return acc	

accS :: Calculation
accS = do
	(Registers ax bx blink acc) <- get
	put (Registers (if blink then ax else acc) (if blink then acc else bx) (if blink then False else True) acc)
	return acc

number :: Int -> Calculation
number x = do
		(Registers ax bx blink acc) <- get
		put (Registers (if blink then ax else x) (if blink then x else bx) (if blink then False else True) acc)
		return acc

commandToCalculation :: String -> Calculation
commandToCalculation s =
  case s of
    "+" -> plus
    "-" -> minus
    "*" -> productS
    "/" -> divS
    "swap" -> swap
    "blink" -> blinkS
    "acc" -> accS
    x -> number (read x)

buildCalculation :: [String] -> Calculation
buildCalculation xs = 
  foldl (\a x -> a >>= (\_ -> x)) (state (\s -> (0, s))) (map commandToCalculation xs)

calculate :: [String] -> Int
calculate xs = evalState (buildCalculation xs) emptyRegisters
