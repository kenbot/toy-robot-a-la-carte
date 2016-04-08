{-# LANGUAGE FlexibleContexts #-}

module ToyRobot.GameScript where

import Control.Monad.Free.Church
import Control.Monad.State
import ToyRobot.Model
import ToyRobot.DataTypesALaCarte



type GameScript = F

type GameMove = StateT Robot IO


runGameScript :: Run f m => GameScript f a -> m a
runGameScript free = iterM runAlgebra free


runGame :: (Run f GameMove) => Robot -> GameScript f a -> IO a
runGame robot script = fst <$> runStateT gameMove robot
  where gameMove = runGameScript script
