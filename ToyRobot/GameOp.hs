{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ToyRobot.GameOp where
  
import ToyRobot.Model  
import ToyRobot.GameScript
import ToyRobot.DataTypesALaCarte
import Control.Monad.State


data GameOp a where
  CurrentRobot :: (Robot -> a) -> GameOp a
  ReportMsg :: String -> a -> GameOp a
  TurnLeft :: a -> GameOp a
  TurnRight :: a -> GameOp a
  Move :: a -> GameOp a
  deriving Functor
  

currentRobot :: (GameOp :<: f) => GameScript f Robot
currentRobot = inject $ CurrentRobot pure

reportMsg :: (GameOp :<: f) => String -> GameScript f ()
reportMsg str = inject $ ReportMsg str (pure ())

turnLeft :: (GameOp :<: f) => GameScript f ()
turnLeft = inject $ TurnLeft (pure ())

turnRight :: (GameOp :<: f) => GameScript f ()
turnRight = inject $ TurnRight (pure ())

move :: (GameOp :<: f) => GameScript f ()
move = inject $ Move (pure ())

report :: (GameOp :<: f) => GameScript f ()
report = currentRobot >>= reportMsg . show


  
instance Show (GameOp a) where 
  show (CurrentRobot f) = "CurrentRobot"
  show (ReportMsg str _) = "Report: " ++ str
  show (Move _) = "Move"
  show (TurnRight _) = "TurnRight"
  show (TurnLeft _) = "TurnLeft"
  

instance Run GameOp GameMove where 
  runAlgebra (CurrentRobot f) = get >>= f
  runAlgebra (ReportMsg str ma) = liftIO (putStrLn str) >> ma
  runAlgebra (TurnLeft ma) = modify turnRobotRight >> ma
  runAlgebra (TurnRight ma) = modify turnRobotLeft >> ma
  runAlgebra (Move ma) = modify moveRobot >> ma