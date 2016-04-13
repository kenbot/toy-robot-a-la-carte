{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ToyRobot.Model where
  
import Control.Lens


type Filename = String

data Facing = U | D | L | R
  deriving (Show, Eq)

type Pos = (Int,Int)

leftwards :: Facing -> Facing
leftwards = intToFacing . pred . facingToInt

rightwards :: Facing -> Facing
rightwards = intToFacing . succ . facingToInt

intToFacing :: Int -> Facing
intToFacing i = case (mod i 4) of 
  0 -> U
  1 -> R
  2 -> D
  3 -> L
  
facingToInt :: Facing -> Int
facingToInt = \case 
  U -> 0
  R -> 1
  D -> 2
  L -> 3



data Robot = Robot 
  { _pos :: Pos
  , _facing :: Facing
  } 
  deriving (Show, Eq)
  
makeLenses ''Robot


movePos :: Facing -> Pos -> Pos
movePos U (x, y) = (x, y-1)
movePos D (x, y) = (x, y+1)
movePos L (x, y) = (x-1, y)
movePos R (x, y) = (x+1, y)


data World = World { _robot :: Robot }
  deriving Show
  
makeLenses ''World


moveRobot :: Robot -> Robot
moveRobot (Robot pos facing) = Robot (movePos facing pos) facing

turnRobotLeft :: Robot -> Robot
turnRobotLeft (Robot pos f) = Robot pos (leftwards f)

turnRobotRight :: Robot -> Robot
turnRobotRight (Robot pos f) = Robot pos (rightwards f)