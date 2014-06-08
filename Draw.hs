module Draw where

import qualified Data.IntMap.Strict as Map
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL hiding (Rect)
import Data.Foldable as Foldbl (mapM_)
import Data.Vec

import GameLoop
import MyGameType

draw :: Drawer GameState
draw fps gs = do
  GL.clear [ColorBuffer, DepthBuffer]
  Foldbl.mapM_ drawEntity (entities gs)
  GLFW.swapBuffers
  return ()

drawEntity :: Entity -> IO ()
drawEntity (Entity (Vec2F x y) _) = do
  drawWhiteBox (x-10) (y-10) 20 20

drawBox :: GL.Color4 GLfloat -> Float -> Float -> Float -> Float -> IO ()
drawBox color fx fy fw fh = do
  let vtx vx vy = GL.vertex $ GL.Vertex2 vx (vy :: GLfloat)
  let x = realToFrac fx
  let y = realToFrac fy
  let w = realToFrac fw
  let h = realToFrac fh

  currentColor $= color
  renderPrimitive Quads $ do
    vtx x     y
    vtx x     (y+h)
    vtx (x+w) (y+h)
    vtx (x+w) y

drawWhiteBox :: Float -> Float -> Float -> Float -> IO ()
drawWhiteBox = drawBox (Color4 1.0 1.0 1.0 1.0)


interpolateGameState :: GameState -> GameState -> Float -> GameState
interpolateGameState (GameState entities0) (GameState entities1) time = GameState $ interpolateOnMap interpolateEntity entities0 entities1 time
--interpolateGameState _ gs _ = gs

interpolate :: Num n => n -> n -> n -> n
interpolate val0 val1 time = (val0 * (1 - time)) + (val1 * time)

interpolateVec :: Vec2F -> Vec2F -> Float -> Vec2F
interpolateVec (Vec2F x0 y0) (Vec2F x1 y1) time = (Vec2F (interpolate x0 x1 time) (interpolate y0 y1 time))

interpolateEntity :: Entity -> Entity -> Float -> Entity
interpolateEntity (Entity pos0 vel0) (Entity pos1 vel1) time = (Entity (interpolateVec pos0 pos1 time) (interpolateVec vel0 vel1 time))

interpolateOnMap :: (a -> a -> Float -> a) -> Map.IntMap a -> Map.IntMap a -> Float -> Map.IntMap a
interpolateOnMap interpFunc mapBefore mapAfter time = newMap
  where
    -- Interpolations can only be applied when both old and new values exist:
    mapWithInterpolations = Map.intersectionWith (\a b -> interpFunc a b time) mapBefore mapAfter
    -- The old map with all the interpolated values overridden
    newMap = Map.union mapWithInterpolations mapBefore
