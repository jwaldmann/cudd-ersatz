{-# language FlexibleContexts #-}

import Prelude hiding ((&&),(||),not,and,or)
import Cudd.Ersatz

import Data.List (sortOn)
import Control.Monad ( guard, forM, replicateM )
import System.Environment ( getArgs )

type Comp = (Int,Int)
type Net = [ Comp ]

comparators n = do
  x <- [0..n-1] ; y <- [x+1..n-1] ; return (x,y)

comp :: Boolean b => [b] -> Comp -> [b]
comp xs (lo,hi) | lo /= hi = xs //
  [ (lo, xs !! lo && xs !! hi)
  , (hi, xs !! lo || xs !! hi)
  ]

xs // kvs = foldl update xs kvs
update xs (k,v) =
  let (pre,this:post) = splitAt k xs
  in  pre ++ v : post

monotone xs = and $ zipWith (==>) xs $ tail xs

main = do
    args <- getArgs
    case map read args :: [Int] of
        [] -> run 10
        [arg] -> run arg

run n = managed $ do
  input <- replicateM n variable
  let g = not $ monotone input
      start = do i <- [0 ,2 .. n-2 ] ; return (i,i+1)
  let output = foldl comp input start
  w <- nom $ g && ( not $ monotone output )
  let go (w,(net,bits)) =
        if w == 0
	then error $ show (length net, net)
	else do
	  wns <- (sortOn fst <$> ) $ forM (comparators n) $ \ c -> do
	    let bits' = comp bits c
	        net' = net ++ [c]
	    w' <- nom $ g && ( not $ monotone bits' )
	    return (w', (net',bits'))
	  go $ head wns
  go (w, (start, output))

nom d = bdd d >>= number_of_models