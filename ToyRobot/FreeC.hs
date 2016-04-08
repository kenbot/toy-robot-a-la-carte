{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module ToyRobot.FreeC where

import Data.Functor.Yoneda
import Data.Functor.Coyoneda
import Control.Monad.Free.Church

type f ~> g = forall a. f a -> g a

type FreeC f = F (Coyoneda f)

liftFC :: f a -> FreeC f a
liftFC = liftF . liftCoyoneda

foldFreeC :: (Monad m) => (f ~> m) -> FreeC f ~> m
foldFreeC nt freeC = foldF (promoteC nt) freeC

promoteC :: (Functor g) => f ~> g -> Coyoneda f ~> g
promoteC nt (Coyoneda f fb) = f <$> nt fb