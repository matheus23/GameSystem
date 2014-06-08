module GameSystem where

import CappedList

type TimeMs = Integer
type Tick = Integer

-- s: Game state datatype
-- i: Input datatype
-- c: datatype for describing Game State changes
data GameSystem s i c = GameSystem {
  statesMemory :: CappedList (Tick, c, s),

  produceUpdate :: s -> i -> c,
  applyUpdate :: s -> c -> s
}

advanceGameSystem :: GameSystem s i c -> i -> GameSystem s i c
advanceGameSystem gameSystem@(GameSystem stateHistory produceUpd applyUpd) input =
  gameSystem { statesMemory = hlPush (tickBefore + 1, changes, newState) stateHistory }
  where
    (tickBefore, _, stateBefore) = hlHead stateHistory
    changes = produceUpd stateBefore input
    newState = applyUpd stateBefore changes

getGameSystemAt :: GameSystem s i c -> i -> Tick -> GameSystem s i c
getGameSystemAt gameSystem@(GameSystem stateHistory _ _) input queryTick
  | tick == queryTick       = gameSystem                         -- If the game state has already been computed
  | tick == (queryTick - 1) = advanceGameSystem gameSystem input -- If the game state needs to be computed, and the last game state is present
  | otherwise               = advanceGameSystem (getGameSystemAt gameSystem input (tick - 1)) input
  where (tick, _, _) = hlHead stateHistory

getGameStateAt :: GameSystem s i c -> i -> Tick -> (s, GameSystem s i c)
getGameStateAt gameSystem input tick = (newState, newMemory)
  where
    newMemory@(GameSystem stateHistory _ _) = getGameSystemAt gameSystem input tick
    (_, _, newState) = hlHead stateHistory

lastAndNextGameState :: Tick -> TimeMs -> GameSystem s i c -> i -> (GameSystem s i c, s, s)
lastAndNextGameState tickTime time gameSystem input = (newSystem, lastState, newState)
  where
    tick = (time `div` tickTime) + 1
    newSystem@(GameSystem stateHistory _ _) = getGameSystemAt gameSystem input tick
    (_, _, newState) = hlHead stateHistory
    (_, _, lastState) = (hlHead . hlTail) stateHistory
