{-# language LambdaCase #-}
{-# language TupleSections #-}

import Prelude hiding ((&&),(||),not,or,and)
import qualified Prelude as P
import Cudd.Ersatz
import Control.Monad (guard, forM, replicateM)
import System.Environment ( getArgs )

main = getArgs >>= \ case
  [] -> run 100
  [s] -> run $ read s

run n = do
  c <- managed $ do
      xs <- replicateM n variable
      d <- bdd $ foldr xor false xs
      number_of_models d
  let e = 2 ^ pred n     
  putStrLn $ unlines [ "size " ++ show n
                     , "number of models " ++ show c
                     , "expected         " ++ show e
                     ]



