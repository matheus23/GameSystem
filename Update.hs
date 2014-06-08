module Update where

import qualified Graphics.UI.GLFW as GLFW
import qualified Data.IntMap.Strict as Map
import Data.Vec

import GameSystem
import CappedList
import GLFWInput
import MyGameType

myGameSystem :: MyGameSystem
myGameSystem = GameSystem stateHistory produceUpd applyUpdsAndSimulate
  where stateHistory = CappedList 20 [(0, NoChanges, initialGameState)]

speed :: Float
speed = 1.0 * (fromIntegral tickTime)

ups :: Integer
ups = 20

tickTime :: TimeMs
tickTime = 1000 `div` ups

produceUpd :: GameState -> UserInput -> GameChangeEvents
produceUpd gs input = Changes $ changeEntity 0 (UpdateVelXY vel)
  where
    up = isSKeyDown GLFW.UP input || isCKeyDown 'W' input
    down = isSKeyDown GLFW.DOWN input || isCKeyDown 'S' input
    left = isSKeyDown GLFW.LEFT input || isCKeyDown 'A' input
    right = isSKeyDown GLFW.RIGHT input || isCKeyDown 'D' input
    xVel = boolToValueDefault left right (-speed) speed 0.0
    yVel = boolToValueDefault up down (-speed) speed 0.0
    vel = Vec2F xVel yVel
    {-mousePos = getMousePos input
    playerPos = pos (getMainEntity gs)
    diff = mousePos - playerPos
    vel = 10 * (normalize diff)-}

boolToValueDefault :: Bool -> Bool-> a -> a -> a -> a
boolToValueDefault True False v _ _ = v
boolToValueDefault False True _ v _ = v
boolToValueDefault _ _ _ _ def = def

applyUpdsAndSimulate :: GameState -> GameChangeEvents -> GameState
applyUpdsAndSimulate gs changes = simulatePhysics $ applyUpds gs changes

applyUpds :: GameState -> GameChangeEvents -> GameState
applyUpds gs NoChanges = gs
applyUpds gs (Changes (firstChange `AndThen` secondChange)) = gs
  `applyUpds` (Changes firstChange)
  `applyUpds` (Changes secondChange)
applyUpds gs (Changes (Single change)) = applyChange gs change

applyChange :: GameState -> GameChangeEvent -> GameState
applyChange (GameState entityMap) (AddEntity entity) = (GameState $ Map.insert (findNextID entityMap) entity entityMap)
applyChange (GameState entityMap) (RemoveEntity entityID) = (GameState $ Map.delete entityID entityMap)
applyChange (GameState entityMap) (ChangeEntity entityID change) = (GameState $ Map.update updateEntity entityID entityMap)
  where updateEntity entity = Just $ applyEntityChange entity change

applyEntityChange :: Entity -> EntityChange -> Entity
applyEntityChange (Entity _ velocity) (UpdatePosXY newPos) = (Entity newPos velocity)
applyEntityChange (Entity position _) (UpdateVelXY newVel) = (Entity position newVel)

findNextID :: Map.IntMap Entity -> EntityID
findNextID entityMap = Map.foldrWithKey (\entityId _ maxEntityId -> max maxEntityId entityId) 0 entityMap

simulatePhysics :: GameState -> GameState
simulatePhysics (GameState entityMap) = (GameState $ Map.mapWithKey simulateEntityPhysics entityMap)

simulateEntityPhysics :: EntityID -> Entity -> Entity
simulateEntityPhysics _ (Entity pos vel) = (Entity (pos + vel) vel)

initialGameEntityMap :: Map.IntMap Entity
initialGameEntityMap = Map.singleton 0 (Entity (Vec2F 100 100) (Vec2F 10 0))

initialGameState :: GameState
initialGameState = GameState initialGameEntityMap
