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
  Save :: World -> a -> FileOp a
  Load :: Filename -> (World -> a) -> FileOp a
  deriving Functor


save :: (FileOp :<: f) => World -> GameScript f ()
save world = inject $ Save world (pure ())

load :: (FileOp :<: f) => Filename -> GameScript f World
load robot = inject $ Load robot pure


instance Show (FileOp a) where
  show (Save _ _) = "Save"
  show (Load filename _) = "Load " ++ filename

instance Run FileOp (StateT World IO) where 
  runAlgebra (Save world ma) = liftIO (putStrLn ("Saving" ++ show world)) >> ma
  runAlgebra (Load filename f) = pure (World (Robot (1,2) U)) >>= f