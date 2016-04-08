{-# LANGUAGE LambdaCase #-}

module ToyRobot.Model where

type Filename = String

data Facing = U | D | L | R
  deriving (Show, Eq)

type Pos = (Int,Int)

leftwards :: Facing -> Facing
leftwards = intToFacing . (\n -> n - 1) . facingToInt

rightwards :: Facing -> Facing
rightwards = intToFacing . (+1) . facingToInt

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


data Robot = Robot Pos Facing
  deriving (Show, Eq)

movePos :: Facing -> Pos -> Pos
movePos U (x, y) = (x, y-1)
movePos D (x, y) = (x, y+1)
movePos L (x, y) = (x-1, y)
movePos R (x, y) = (x+1, y)

moveRobot :: Robot -> Robot
moveRobot (Robot pos facing) = Robot (movePos facing pos) facing

turnRobotLeft :: Robot -> Robot
turnRobotLeft (Robot pos f) = Robot pos (leftwards f)

turnRobotRight :: Robot -> Robot
turnRobotRight (Robot pos f) = Robot pos (rightwards f)