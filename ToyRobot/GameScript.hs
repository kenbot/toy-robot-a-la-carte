{-# LANGUAGE FlexibleContexts #-}

module ToyRobot.GameScript where

import Control.Monad.Free.Church
import Control.Monad.State
import ToyRobot.Model
import ToyRobot.DataTypesALaCarte
import ToyRobot.FreeC


type GameScript = F

type GameMove = StateT World IO


runGameScript :: Run f m => GameScript f a -> m a
runGameScript free = iterM runAlgebra free


runGame :: (Run f GameMove) => World -> GameScript f a -> IO a
runGame world script = fst <$> runStateT gameMove world
  where gameMove = runGameScript script
