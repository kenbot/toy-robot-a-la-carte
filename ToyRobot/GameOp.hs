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
import Control.Lens


data GameOp a where
  GetWorld :: (World -> a) -> GameOp a
  ReportMsg :: String -> a -> GameOp a
  TurnLeft :: a -> GameOp a
  TurnRight :: a -> GameOp a
  Move :: a -> GameOp a
  deriving Functor
  

getWorld :: (GameOp :<: f) => GameScript f World
getWorld = inject $ GetWorld pure

reportMsg :: (GameOp :<: f) => String -> GameScript f ()
reportMsg str = inject $ ReportMsg str (pure ())

turnLeft :: (GameOp :<: f) => GameScript f ()
turnLeft = inject $ TurnLeft (pure ())

turnRight :: (GameOp :<: f) => GameScript f ()
turnRight = inject $ TurnRight (pure ())

move :: (GameOp :<: f) => GameScript f ()
move = inject $ Move (pure ())

report :: (GameOp :<: f) => GameScript f ()
report = getWorld >>= reportMsg . show


  
instance Show (GameOp a) where 
  show (GetWorld f) = "CurrentRobot"
  show (ReportMsg str _) = "Report: " ++ str
  show (Move _) = "Move"
  show (TurnRight _) = "TurnRight"
  show (TurnLeft _) = "TurnLeft"
  

instance Run GameOp GameMove where 
  runAlgebra (GetWorld f) = get >>= f
  runAlgebra (ReportMsg str ma) = liftIO (putStrLn str) >> ma
  runAlgebra (TurnLeft ma) = (robot.facing %= leftwards) >> ma
  runAlgebra (TurnRight ma) = (robot.facing %= rightwards) >> ma
  runAlgebra (Move ma) = (robot %= moveRobot) >> ma