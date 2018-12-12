{-# language ImplicitParams #-}

import Prelude hiding ((&&),(||),not,and,or)
import Cudd.Ersatz

import qualified Data.Foldable as F
import Control.Monad ( guard, when, replicateM, forM_ )
import System.Environment ( getArgs )
import qualified Data.Array as A
import qualified Data.Map.Strict as M



main = do
    args <- getArgs
    case map read args :: [Int] of
        [] -> mainf 10 5
        [arg] -> forM_ [0 ..arg] $ \ k -> mainf arg k
        [n, k] -> mainf n k

mainf :: Int -> Int -> IO ()
mainf n k = do
    c <- managed $ do
      xs <- replicateM n variable
      d <- bdd $ exactly k xs
      number_of_models d
    let n' = fromIntegral n :: Integer
        expected = div (product $ take k [ n', n' - 1 ..  ])
                       (product $ take k [ 1 ..  ]) :: Integer
    putStrLn $ unwords [ show n, "choose", show k
                       , "BDD model count", show c
                       , "actual value", show expected
                       , "difference", show (c - expected)
                       ]
    when ( c /= expected ) $ error "huh"


exactly :: Boolean v => Int -> [v] -> v
exactly k xs = equals (encode k) $ count $ F.toList xs

encode i = case i of
  0 -> []
  _ | i > 0 -> 
    let (d,m) = divMod i 2 
    in  bool (m == 1) : encode d

equals [] []  = true
equals xs [] = not  $ or xs
equals [] ys = not $ or ys
equals (x:xs) (y:ys) = not (xor x y) && equals xs ys

count [] = encode 0
count [x] = [x]
count xs = 
  let (ys,zs) = splits xs in plus (count ys) (count zs)

splits [] = ([],[])
splits (x:xs) = 
  let (ys,zs) = splits xs in (x:zs,ys)

plus xs ys = 
  let go a [] [] = [a]
      go a (x:xs) [] = 
        let (s,c) = halfAdder a x in s : go c xs []
      go a [] (y:ys) = 
        let (s,c) = halfAdder a y in s : go c [] ys
      go a (x:xs) (y:ys) = 
        let (s,c) = fullAdder a x y in s : go c xs ys
  in  go false xs ys

halfAdder x y = (xor x y, x && y)
fullAdder x y z = 
  let (s1,c1) = halfAdder x y
      (s2,c2) = halfAdder s1 z
  in  (s2,c1 || c2)
