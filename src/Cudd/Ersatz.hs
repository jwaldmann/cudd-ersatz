-- | An Ersatz-like interface to CUDD.

{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}

module Cudd.Ersatz

( Boolean (..)
, variable, boolean_array
, managed, bdd, number_of_models
)
       
where

import Ersatz.Bit (Boolean(..))
import qualified Cudd.Imperative as C
import qualified Cudd.C as C
import Foreign

import Prelude hiding ((||),(&&),not,and,or)
import qualified Data.Foldable (foldl')
import Control.Monad.State
  (MonadState, StateT, evalStateT, lift, liftM2, liftM3)
import Control.Monad.ST
import Control.Monad ( forM)

import Cudd.Ersatz.Internal.StableName
import System.IO.Unsafe
import Control.Monad.ST.Unsafe
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Control.Lens
import qualified Data.Array as A

-- newtype CUDD s u a = CUDD (ReaderT (C.DDManager s u) (ST s) a)
--   deriving (Functor, Applicative, Monad)

type CUDD s u a = StateT (State s u) (ST s) a

data State s u = State
   { _stableMap :: !(HashMap (StableName ()) (C.DDNode s u))
   , _manager :: !(C.DDManager s u)
   , _next :: !Int
   } 

stableMap :: Lens' (State s u)
                   (HashMap (StableName ()) (C.DDNode s u))
stableMap f (State s m n) = fmap (\ s' -> State s' m n) (f s)

manager :: Lens' (State s u)
                 (C.DDManager s u)
manager f (State s m n) = fmap (\ m' -> State s m' n) (f m)

next :: Lens' (State s u)
                 Int
next f (State s m n) = fmap (\ n' -> State s m n') (f n)


-- | Run an action under control of one (fresh) manager.
-- The BDDs constructed inside (with @bdd@) will share variables
-- and nodes.
managed :: (forall u . CUDD RealWorld u a)
        -> IO a
managed action =
  stToIO $ C.withManagerDefaults $ \ manager ->
     evalStateT action (State HashMap.empty manager 0)

-- | Creates a node representing a fresh variable.
variable :: CUDD s u (Bit (C.DDNode s u))
variable = use manager >>= \ m -> do
  next += 1
  Node <$> lift ( C.newVar m )

-- | Creates an array of variables, with given bounds.
boolean_array bnd =
  A.array bnd <$> forM (A.range bnd) ( \ i -> (i,) <$> variable )

-- | Constructs the BDD for the given formula.
-- It is recommended that the formula has type @Boolean b => b@.
bdd b = node b $ \ case 
  Node n -> return n
  Constant False -> use manager >>= \ m -> return $ C.bZero m
  Constant True  -> use manager >>= \ m -> return $ C.bOne  m
  Not a -> C.bNot <$> bdd a
  And x y -> fun2 C.bAnd x y
  Or  x y -> fun2 C.bOr x y
  Xor  x y -> fun2 C.bXor x y
  Choose n y f -> fun3 C.bIte f y n
  Implies x y -> fun3 C.bIte x y true

fun2 f x y = use manager >>= \ m ->
    do a <- bdd x ; b <- bdd y ; lift $ f m a b
fun3 f x y z = use manager >>= \ m ->
    do a <- bdd x ; b <- bdd y ; c <- bdd z ; lift $ f m a b c

-- | Walks the AST. Detects identical subtrees,
-- via their stable names.
node ::  MonadState (State s u) m
     =>  t
     -> (t -> m (C.DDNode s u))
     -> m (C.DDNode s u)
node a f = do
  let sn = unsafePerformIO (makeStableName' a)
  use (stableMap.at sn) >>= \ mn -> case mn of
    Just n -> return n
    Nothing -> do
      n <- f a
      stableMap.at sn ?= n
      return n

-- | returns the number of models (minterms) of the BDD
number_of_models d = use manager >>= \ m -> use next >>= \ n -> do
  lift $ bCountMinterm m d n

bCountMinterm (C.DDManager m) (C.DDNode d) v =
  realToFrac <$> unsafeIOToST ( 
    C.c_cuddCountMinterm m d (fromIntegral v) )


-- | Abstract syntax tree of propositional logic formula.
data Bit n
  = Node !n
  | Constant !Prelude.Bool
  | Not !(Bit n)
  | And !(Bit n) !(Bit n)
  | Or !(Bit n) !(Bit n)
  | Xor !(Bit n) !(Bit n)
  | Implies !(Bit n) !(Bit n)
  | Choose !(Bit n) !(Bit n) !(Bit n)

instance Boolean (Bit n) where
  bool = Constant
  not = Not ; (&&) = And ; (||) = Or ; xor = Xor
  all p = Data.Foldable.foldl' (\res b -> res && p b) true
  any p = Data.Foldable.foldl' (\res b -> res || p b) false
  choose = Choose
  (==>) = Implies
