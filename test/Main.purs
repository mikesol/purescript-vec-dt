module Test.Main where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Data.DT.Vec (Unk0', assertEq, toList, vec', zipWithE)
import Data.Functor.Indexed (class IxFunctor)
import Data.List (fromFoldable)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (Error, error)

foreign import arr0 :: Effect (Array Int)
foreign import arr1 :: Effect (Array Int)
foreign import arr2 :: Effect (Array Int)

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

main :: Effect Unit
main = do
  list0 <- fromFoldable <$> arr0
  list1 <- fromFoldable <$> arr1
  list2 <- fromFoldable <$> arr2
  let 
    (Ctxt toPrint) = Ix.do
          ipure unit :: Ctxt Unk0' Unk0' Unit
          v0 <- vec' list0
          v1 <- vec' list1
          v2 <- vec' list2
          l /\ m <- assertEq (error "not eq") (v0 /\ v1)
          _ /\ r <- assertEq (error "not eq") (m /\ v2)
          ipure $ zipWithE (+) l r
  toPrint >>= log <<< show <<< toList
