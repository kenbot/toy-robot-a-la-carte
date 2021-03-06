{-# LANGUAGE TypeOperators #-}


module Main where
  
import ToyRobot.Model
import ToyRobot.GameOp
import ToyRobot.FileOp
import ToyRobot.GameScript
import ToyRobot.DataTypesALaCarte


main :: IO ()
main = runGame (World (Robot (1,1) U)) program

program :: GameScript (GameOp :+: FileOp) () 
program = 
  do
    report
    move 
    turnLeft
    getWorld >>= save
    





   