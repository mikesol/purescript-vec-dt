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
  , VecSig'
  , class AssertEq
  , class Unkable
  , class BalanceExpr
  , class EqExpr
  , class Leftmost
  , consVec
  , appendVec
  , vec'
  , assertEq
  , zipWithE
  , replicate
  , (+>)
  , (<+>)
  , fill
  , toList
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.List (List(..), (:))
import Data.List as L
import Data.Tuple.Nested ((/\), type (/\))
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
class Unkable k (u :: Unk') | k -> u

instance unkableUnkable0 :: Unkable Unk0' Unk0'
instance unkableUnkableX :: Unkable (UnkX' a) (UnkX' a)


instance lNatA :: Leftmost Zero' Zero' Zero' Zero'

instance lNatB :: Leftmost (After' x) Zero' Zero' (After' x)

instance lNatC :: Leftmost Zero' (After' x) Zero' (After' x)

instance lNatD :: Leftmost a b x y => Leftmost (After' a) (After' b) x y

instance lUnkA :: Leftmost Unk0' Unk0' Unk0' Unk0'

instance lUnkB :: Leftmost (UnkX' x) Unk0' Unk0' (UnkX' x)

instance lUnkC :: Leftmost Unk0' (UnkX' x) Unk0' (UnkX' x)

instance lUnkD :: Leftmost a b x y => Leftmost (UnkX' a) (UnkX' b) x y

class Leftmost :: forall k. k -> k -> k -> k -> Constraint
class Leftmost a b c d | a b -> c d

instance leftmostLeftmostA :: Leftmost (Const' a) (Var' b) (Var' b) (Const' a)

instance leftmostLeftmostB :: Leftmost (Var' b) (Const' a) (Var' b) (Const' a)

instance leftmostLeftmostC :: Leftmost a b x y => Leftmost (Const' a) (Const' b) (Const' x) (Const' y)

instance leftmostLeftmostD :: Leftmost a b x y => Leftmost (Var' b) (Var' a) (Var' x) (Var' y)

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
type VecSig' :: forall ix ox. Type -> (ix -> ox -> Type -> Type) -> Unk' -> ix -> ox -> Type
type VecSig' a m (u :: Unk') i o = Unkable i u => Unkable o (UnkX' u) => Applicative (m i o) => List a -> m i o (Vec (Var' u) a)
vec' :: forall a m (u :: Unk') i o. VecSig' a m u i o
vec' = pure <<< Vec

leq :: forall a. Array (List a) -> Boolean
leq = go <<< L.fromFoldable
  where
  go Nil = true
  go (a : Nil) = true
  go (a : b : c) = L.length a == L.length b && go (b : c)
-- | Construct a vector of unknown length
-- |
class AssertEq i o | i -> o where
  assertEq :: forall (a :: Type) (err :: Type) (m :: Type -> Type).  MonadThrow err m => err -> i -> m o

instance assertEq0 :: AssertEq (Vec j a /\ Vec k a) (Vec j a /\ Vec j a) where
  assertEq err (Vec a /\ Vec b)
    | leq [a,b] = pure $ Vec a /\ Vec b
    | otherwise = throwError err
instance assertEq1 :: AssertEq (Vec j a /\ Vec k a /\ Vec l a) (Vec j a /\ Vec j a  /\ Vec j a) where
  assertEq err (Vec a /\ Vec b /\ Vec c)
    | leq [a,b,c] = pure $ Vec a /\ Vec b  /\ Vec c
    | otherwise = throwError err
instance assertEq2 :: AssertEq (Vec j a /\ Vec k a  /\ Vec l a /\ Vec m a) (Vec j a /\ Vec j a  /\ Vec j a /\ Vec j a) where
  assertEq err (Vec a /\ Vec b /\ Vec c /\ Vec d)
    | leq [a,b,c,d] = pure $ Vec a /\ Vec b  /\ Vec c /\ Vec d
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

fill :: forall (a :: Expr') x. Vec a x -> Function Int ~> Vec a
fill (Vec l) f = Vec (f <$> L.range 0 (L.length l))

toList :: forall (a :: Expr'). Vec a ~> L.List
toList (Vec l) = l
