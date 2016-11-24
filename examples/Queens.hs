{-# language ImplicitParams #-}

import Prelude hiding ((&&),(||),not,and,or)
import Cudd.Ersatz

import Control.Monad ( guard )
import System.Environment ( getArgs )
import qualified Data.Array as A
import qualified Data.Map.Strict as M

board :: Boolean b => A.Array (Int,Int) b -> b
board q = and 
    [ handle exactlyone (\(x,y) -> x) q
    , handle atmostone  (\(x,y) -> y) q
    , handle atmostone  (\(x,y) -> x+y) q
    , handle atmostone  (\(x,y) -> x-y) q
    ]

atmostone :: Boolean b => [b]  -> b
atmostone xs =
  let go (n,o) [] = n || o
      go (n,o) (x:xs) = go (n && not x, choose o n x) xs
  in  go (true,false) xs

exactlyone :: Boolean b => [b]  -> b
exactlyone xs =
  let go (n,o) [] = o
      go (n,o) (x:xs) = go (n && not x, choose o n x) xs
  in  go (true,false) xs

handle :: (A.Ix i , Boolean b, Ord c )
       => ([b] -> b)
       -> (i -> c)
       -> A.Array  i b
       -> b
handle check f q = and $ do
    (k,v) <- M.toList $ M.fromListWith (++)
          $ map (\(p,v) -> (f p, [v]))
          $ A.assocs q
    return $ check v

main = do
    args <- getArgs
    case map read args :: [Int] of
        [] -> mainf 10
        [arg] -> mainf arg

mainf n = do
    c <- managed $ do
      q <- boolean_array ((1,1),(n,n)) 
      d <- bdd $ board q
      number_of_models d
    putStrLn $ unwords [ "board size", show n 
                       , "number of models", show c ]
