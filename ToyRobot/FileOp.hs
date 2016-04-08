{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ToyRobot.FileOp where
  
import ToyRobot.Model  
import ToyRobot.GameScript
import ToyRobot.DataTypesALaCarte
import Control.Monad.State

data FileOp a where
  Save :: Robot -> a -> FileOp a
  Load :: Filename -> (Robot -> a) -> FileOp a
  deriving Functor


save :: (FileOp :<: f) => Robot -> GameScript f ()
save robot = inject $ Save robot (pure ())

load :: (FileOp :<: f) => Filename -> GameScript f Robot
load robot = inject $ Load robot pure


instance Show (FileOp a) where
  show (Save _ _) = "Save"
  show (Load filename _) = "Load " ++ filename

instance Run FileOp (StateT Robot IO) where 
  runAlgebra (Save robot ma) = liftIO (putStrLn ("Saving" ++ show robot)) >> ma
  runAlgebra (Load filename f) = pure (Robot (1,2) U) >>= f