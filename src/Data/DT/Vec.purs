module Data.DT.Vec
  ( Unk0'
  , Unk'
  , Expr'
  , Vec
  , Plus'
  , Const'
  , After'
  , Zero'
  , Nat'
  , UnkX'
  , Var'
  , class Unkable
  , class BalanceExpr
  , class EqExpr
  , class Leftmost
  , class LUnk
  , class LNat
  , consVec
  , appendVec
  , vec
  , assertEq2
  , zipWithE
  , replicate
  , (+>)
  , (<+>)
  , fill
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.List (List(..))
import Data.List as L
import Data.Tuple.Nested((/\), type (/\))
import Data.Unfoldable as U

data Nat'

foreign import data After' :: Nat' -> Nat'

foreign import data Zero' :: Nat'

data Unk'

foreign import data UnkX' :: Unk' -> Unk'

foreign import data Unk0' :: Unk'

data Expr'

foreign import data Const' :: Nat' -> Expr'

foreign import data Var' :: Unk' -> Expr'

foreign import data Plus' :: Expr' -> Expr' -> Expr'

data Nat
  = After Nat
  | Zero

newtype Vec (n :: Expr') a
  = Vec (List a)

class Unkable :: forall k. k -> Unk' -> Constraint
class Unkable k (u :: Unk') | k -> u, u -> k

instance unkableUnkable0 :: Unkable Unk0' Unk0'
instance unkableUnkableX :: Unkable (UnkX' a) (UnkX' a)



class LNat (a :: Nat') (b :: Nat') (c :: Nat') (d :: Nat') | a b -> c d

instance lNatA :: LNat Zero' Zero' Zero' Zero'

instance lNatB :: LNat (After' x) Zero' Zero' (After' x)

instance lNatC :: LNat Zero' (After' x) Zero' (After' x)

instance lNatD :: LNat a b x y => LNat (After' a) (After' b) x y

class LUnk (a :: Unk') (b :: Unk') (c :: Unk') (d :: Unk') | a b -> c d

instance lUnkA :: LUnk Unk0' Unk0' Unk0' Unk0'

instance lUnkB :: LUnk (UnkX' x) Unk0' Unk0' (UnkX' x)

instance lUnkC :: LUnk Unk0' (UnkX' x) Unk0' (UnkX' x)

instance lUnkD :: LUnk a b x y => LUnk (UnkX' a) (UnkX' b) x y

class Leftmost (a :: Expr') (b :: Expr') (c :: Expr') (d :: Expr') | a b -> c d

instance leftmostLeftmostA :: Leftmost (Const' a) (Var' b) (Var' b) (Const' a)

instance leftmostLeftmostB :: Leftmost (Var' b) (Const' a) (Var' b) (Const' a)

instance leftmostLeftmostC :: LNat a b x y => Leftmost (Const' a) (Const' b) (Const' x) (Const' y)

instance leftmostLeftmostD :: LUnk a b x y => Leftmost (Var' b) (Var' a) (Var' x) (Var' y)

class BalanceExpr (a :: Expr') (b :: Expr') | a -> b

instance balanceExprConst :: BalanceExpr (Const' a) (Const' a)

instance balanceExprVar :: BalanceExpr (Var' a) (Var' a)

instance balanceExprPlus ::
  ( BalanceExpr (Plus' m n) (Plus' a' a'')
  , BalanceExpr (Plus' o p) (Plus' b' b'')
  , Leftmost a' b' x y
  , BalanceExpr (Plus' y (Plus' a'' b'')) z
  ) =>
  BalanceExpr (Plus' (Plus' m n) (Plus' o p)) (Plus' x z)

instance balanceExprConstL ::
  ( BalanceExpr (Plus' o p) (Plus' a' a'')
  , Leftmost (Const' a) a' x y
  , BalanceExpr (Plus' y a'') z
  ) =>
  BalanceExpr (Plus' (Const' a) (Plus' o p)) (Plus' x z)

instance balanceExprVarL ::
  ( BalanceExpr (Plus' o p) (Plus' a' a'')
  , Leftmost (Var' a) a' x y
  , BalanceExpr (Plus' y a'') z
  ) =>
  BalanceExpr (Plus' (Var' a) (Plus' o p)) (Plus' x z)

instance balanceExprConstR ::
  ( BalanceExpr (Plus' o p) (Plus' a' a'')
  , Leftmost (Const' a) a' x y
  , BalanceExpr (Plus' y a'') z
  ) =>
  BalanceExpr (Plus' (Plus' o p) (Const' a)) (Plus' x z)

instance balanceExprVarR ::
  ( BalanceExpr (Plus' o p) (Plus' a' a'')
  , Leftmost (Var' a) a' x y
  , BalanceExpr (Plus' y a'') z
  ) =>
  BalanceExpr (Plus' (Plus' o p) (Var' a)) (Plus' x z)

instance balanceExprVarVar ::
  ( Leftmost (Var' a) (Var' b) x y
    ) =>
  BalanceExpr (Plus' (Var' a) (Var' b)) (Plus' x y)

instance balanceExprConstConst ::
  ( Leftmost (Const' a) (Const' b) x y
    ) =>
  BalanceExpr (Plus' (Const' a) (Const' b)) (Plus' x y)

instance balanceExprConstVar ::
  BalanceExpr (Plus' (Const' a) (Var' b)) (Plus' (Var' b) (Const' a))

instance balanceExprVarConst ::
  BalanceExpr (Plus' (Var' b) (Const' a)) (Plus' (Var' b) (Const' a))

class EqExpr (a :: Expr') (b :: Expr')

instance eqExpr :: (BalanceExpr a c, BalanceExpr b c) => EqExpr a b

-- | Construct a vector of unknown length
-- |
vec :: forall a m (u :: Unk') i o. Unkable i u => Unkable o (UnkX' u) => Applicative (m i o) => List a -> m i o (Vec (Var' u) a)
vec = pure <<< Vec


-- | Construct a vector of unknown length
-- |
assertEq2 :: forall a err m (j :: Expr') (k :: Expr').  MonadThrow err m => err -> Vec j a -> Vec k a -> m ((Vec j a) /\ (Vec j a))
assertEq2 err (Vec a) (Vec b)
  | L.length a == L.length b = pure $ (Vec a) /\ (Vec b)
  | otherwise = throwError err

consVec :: forall (a :: Expr') x. x -> Vec a x -> Vec (Plus' (Const' (After' Zero')) a) x
consVec a (Vec x) = Vec (Cons a x)

infixr 4 consVec as +>

appendVec :: forall (a :: Expr') (b :: Expr') x. Vec a x -> Vec b x -> Vec (Plus' a b) x
appendVec (Vec x) (Vec y) = Vec (x <> x)

infixr 4 appendVec as <+>

nil :: forall x. Vec (Const' Zero') x
nil = Vec Nil

zipWithE :: forall (a :: Expr') (b :: Expr') x y z. EqExpr a b => (x -> y -> z) -> Vec a x -> Vec b y -> Vec a z
zipWithE f (Vec a) (Vec b) = Vec (L.zipWith f a b)

replicate :: forall (a :: Expr') x y. Vec a y -> x -> Vec a x
replicate (Vec l) i = Vec (U.replicate (L.length l) i)

fill :: forall (a :: Expr') x b. Vec a x -> (Int -> b) -> Vec a b
fill (Vec l) f = Vec (f <$> L.range 0 (L.length l))
