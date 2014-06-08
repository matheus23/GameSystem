module MyGameType where

import qualified Data.IntMap.Strict as Map
import Data.Vec
import Data.Maybe

import GameSystem
import GLFWInput

type EntityID = Int
type Pos = Vec2F
type Vel = Vec2F

data GameState = GameState {
  entities :: Map.IntMap Entity
}

data Entity = Entity {
  pos :: Pos,
  vel :: Vel
}

data GameChangeEvents = Changes (Chain GameChangeEvent) | NoChanges

data Chain x = (Chain x) `AndThen` (Chain x) | Single x

data GameChangeEvent
  = AddEntity Entity
  | RemoveEntity EntityID
  | ChangeEntity EntityID EntityChange

addEntity :: Entity -> Chain GameChangeEvent
addEntity entity = Single (AddEntity entity)
removeEntity :: EntityID -> Chain GameChangeEvent
removeEntity entityID = Single (RemoveEntity entityID)
changeEntity :: EntityID -> EntityChange -> Chain GameChangeEvent
changeEntity entityID change = Single (ChangeEntity entityID change)

getMainEntity :: GameState -> Entity
getMainEntity gameState = fromJust $ Map.lookup 0 (entities gameState)

data EntityChange
  = UpdatePosXY Pos
  | UpdateVelXY Vel

type MyGameSystem = GameSystem GameState UserInput GameChangeEvents