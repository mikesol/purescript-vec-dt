module Test.Main where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Data.DT.Vec (After', Const', Plus', Unk', Unk0', UnkX', Var', Vec, VecSig', Zero', assertEq, fill, toList, vec', zipWithE, (+>), (<+>))
import Data.Functor.Indexed (class IxFunctor)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (Error, error)

vec :: forall a m (u :: Unk'). VecSig' a m u u (UnkX' u)
vec = vec' :: VecSig' a m u u (UnkX' u)

newtype Ctxt :: forall k1 k2. k1 -> k2 -> Type -> Type
newtype Ctxt i o a
  = Ctxt (Effect a)
derive instance newtypeCtxt :: Newtype (Ctxt i o a) _
derive newtype instance freeProgramFunctor :: Functor (Ctxt i o)
derive newtype instance freeProgramApply :: Apply (Ctxt i o)
derive newtype instance freeProgramBind :: Bind (Ctxt i o)
derive newtype instance freeProgramApplicative :: Applicative (Ctxt i o)
derive newtype instance freeProgramMonad :: Monad (Ctxt i o)
derive newtype instance freeProgramMonadThrow :: MonadThrow Error (Ctxt i o)
instance freeProgramIxFunctor :: IxFunctor Ctxt where
  imap f (Ctxt a) = Ctxt (f <$> a)
instance freeProgramIxApplicative :: IxApply Ctxt where
  iapply (Ctxt f) (Ctxt a) = Ctxt (f <*> a)
instance freeProgramIxApply :: IxApplicative Ctxt where
  ipure a = Ctxt $ pure a
instance freeProgramIxBind :: IxBind Ctxt where
  ibind (Ctxt monad) function = Ctxt (monad >>= (unwrap <<< function))
instance freeProgramIxMonad :: IxMonad Ctxt

test0 :: ∀ (u ∷ Unk') i. Semiring i => List i → List i → Ctxt u (UnkX' (UnkX' u)) (Vec (Var' u) i)
test0 list0 list1 = Ix.do
  v0 <- vec list0
  v1 <- vec list1
  l /\ r <- assertEq (error "not eq") (v0 /\ v1)
  ipure $ zipWithE (+) l r

-----
-- doesn't compile because we haven't asserted equality

{-
test1 (list0 :: List Int) (list1 :: List Int) = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  ipure $ zipWithE (+) v0 v1
-}
-- compiles because the vec cons operation preserves equality
test2 :: ∀ (u ∷ Unk'). List Int → List Int → Ctxt u (UnkX' (UnkX' u)) (Vec (Plus' (Const' (After' Zero')) (Var' u)) Int)
test2 list0 list1 = Ix.do
  v0 <- vec list0
  v1 <- vec list1
  l /\ r <- assertEq (error "not eq") (v0 /\ v1)
  ipure $ zipWithE (+) (1 +> l) (5 +> r)
-- compiles because the vec append operation preserves equality

test3 :: ∀ (u ∷ Unk'). List Int → List Int → List Int → Ctxt Unk0' u (Vec (Plus' (Var' (UnkX' (UnkX' Unk0'))) (Var' Unk0')) Int)
test3 list0 list1 list2 = Ix.do
  v0 <- vec list0
  v1 <- vec list1
  v2 <- vec list2
  l /\ r <- assertEq (error "not eq") (v0 /\ v1)
  ipure $ zipWithE (+) (v2 <+> l) (v2 <+> r)
-- does not compile because the cons operation was only applied to l, not to r
{-
test4 (list0 :: List Int) (list1 :: List Int) = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  l /\ r <- assertEq (error "not eq") (v0 /\ v1)
  ipure $ zipWithE (+) (1 +> l) r
-}
-- does not compile because the append operation was only applied to l, not to r
{-
test5 (list0 :: List Int) (list1 :: List Int) (list2 :: List Int) = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  v2 <- vec list2
  l /\ r <- assertEq (error "not eq") (v0 /\ v1)
  ipure $ zipWithE (+) (v2 <+> l) r
-}
test6 (list0 :: List Int) (list1 :: List Int) (list2 :: List Int) (list3 :: List Int) = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  v2 <- vec list2
  v3 <- vec list3
  l /\ r <- assertEq (error "not eq") (v0 /\ v1)
  x /\ y <- assertEq (error "not eq") (v2 /\ v3)
  ipure $ zipWithE (+) (l <+> y) (x <+> r)

-- also compiles
test7 (list0 :: List Int) (list1 :: List Int) = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  l /\ r <- assertEq (error "not eq") (v0 /\ v1)
  let repl = fill r $ const 5
  let repr = fill l $ const 6
  ipure $ zipWithE (+) (repl <+> l) (repr <+> r)


test8 (list0 :: List Int) (list1 :: List Int) (list2 :: List Int) = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  v2 <- vec list2
  l /\ m /\ r <- assertEq (error "not eq") (v0 /\ v1 /\ v2)
  let step1 = zipWithE (+) l r
  ipure $ zipWithE (+) step1 m

test9 (list0 :: List Int) (list1 :: List Int) (list2 :: List Int) = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  v2 <- vec list2
  l /\ m <- assertEq (error "not eq") (v0 /\ v1)
  _ /\ r <- assertEq (error "not eq") (l /\ v2)
  let step1 = zipWithE (+) l r
  ipure $ zipWithE (+) step1 m

{-
-- won't work as the equality graph is not transitive
test10 (list0 :: List Int) (list1 :: List Int) (list2 :: List Int) = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  v2 <- vec list2
  l /\ m <- assertEq (error "not eq") (v0 /\ v1)
  _ /\ r <- assertEq (error "not eq") (v1 /\ v2)
  let step1 = zipWithE (+) l r
  ipure $ zipWithE (+) step1 m
-}

test11 (list0 :: List Int) (list1 :: List Int) = Ix.do
  ipure unit :: Ctxt Unk0' Unk0' Unit
  v0 <- vec list0
  v1 <- vec list1
  l /\ r <- assertEq (error "not eq") (v0 /\ v1)
  ipure $ zipWithE (+) (zipWithE (+) l r) (fill l (const 0))

main :: Effect Unit
main = do
  let 
    (Ctxt toPrint) = test0 (1 : 2 : Nil) (3 : 4 : Nil)
  toPrint >>= log <<< show <<< toList