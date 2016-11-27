-- | http://oeis.org/A004003

{-# language LambdaCase #-}
{-# language TupleSections #-}

import Prelude hiding ((&&),(||),not,or,and)
import qualified Prelude as P
import Cudd.Ersatz
import qualified Data.Set as S
import Control.Monad (guard, forM)
import System.Environment ( getArgs )

main = getArgs >>= \ case
  [] -> run 8
  [s] -> run $ read s

run n = do
  c <- managed $ do
      tvs <- forM (S.toList $ tiles n) $ \ t -> (t,) <$> variable
      d <- bdd $ tiling n tvs
      number_of_models d
  putStrLn $ unwords [ "board size", show n 
                     , "number of models (tilings)", show c ]


type P = (Int,Int)
type T = S.Set P

tiling n tvs = and $ do
  p <- S.toList $ positions n
  return $ exactly_one $ do
    (t,v) <- tvs
    guard $ S.member p t 
    return v

exactly_one xs = or xs && and ( do
  i <- [0 .. length xs-1]
  j <- [i+1 .. length xs -1 ]
  return $ not (xs !! i) || not ( xs !! j )
                              )

positions :: Int -> S.Set P
positions n = S.fromList $ do
  x <- [1..n] ; y <- [1..n] ; return (x,y)

tiles :: Int -> S.Set T
tiles n = S.union (horizontal n) (vertical n)

horizontal :: Int -> S.Set T
horizontal n = S.fromList $ do
  x <- [1..n-1] ; y <- [1..n] ; return $ S.fromList [(x,y),(x+1,y)]

vertical :: Int -> S.Set T
vertical n = S.fromList $ do
  x <- [1..n] ; y <- [1..n-1] ; return $ S.fromList [(x,y),(x,y+1)]


