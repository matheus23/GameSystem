module GameLoop where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (get)
import Data.IORef
import Control.Concurrent (threadDelay)

import GameSystem
import Loop

type FPS = Float
type Interpolator s = (s -> s -> Float -> s)
type Drawer s = (FPS -> s -> IO ())
type CloseRequested s i = (s -> i -> Bool)

data FramerateValues = FramerateValues {
  fvLastTimeCounterReset :: TimeMs,
  fvFramesCount :: Int,
  fvLastFps :: FPS
}
initialFramerateValues :: FramerateValues
initialFramerateValues = FramerateValues 0 0 0

gameLoop :: IORef i -> Tick -> CloseRequested s i -> Interpolator s -> Drawer s -> GameSystem s i c -> IO (GameSystem s i c)
gameLoop inputRef tickTime closeReq interpolator draw gameSystem = do
  startTime <- currentTimeMs
  fvRef <- newIORef initialFramerateValues
  ioLoop (recGameLoop fvRef inputRef tickTime startTime closeReq interpolator draw) gameSystem

recGameLoop :: IORef FramerateValues -> IORef i -> Tick -> TimeMs -> CloseRequested s i -> Interpolator s -> Drawer s -> GameSystem s i c -> IO (Bool, GameSystem s i c)
recGameLoop fvRef inputRef tickTime startTime closeReq gsInterpolator drawGameState gameSystem = do
  input <- readIORef inputRef
  currentTime <- currentTimeMs
  fps <- countFrame fvRef currentTime
  let time = currentTime - startTime
  let betweenProgression = (fromIntegral (time `mod` tickTime) / fromIntegral tickTime) :: Float
  let (newSystem, gsBefore, gsNow) = lastAndNextGameState tickTime time gameSystem input
  let gsInterpolated = gsInterpolator gsBefore gsNow betweenProgression
  drawGameState fps gsInterpolated
  threadDelay (10000)
  putStrLn $
    "fps: " ++ show fps ++
    ", tick: " ++ show (time `div` tickTime) ++
    ", current time: " ++ show time ++
    ", betweenProgression: " ++ show betweenProgression
  return (not $ closeReq gsInterpolated input, newSystem)

currentTimeMs :: IO TimeMs
currentTimeMs = do
  currentTime <- get GLFW.time
  return $ floor $ currentTime * 1000

countFrame :: IORef FramerateValues -> TimeMs -> IO FPS
countFrame fvRef currentTime = do
  (FramerateValues lastTime count lastFps) <- readIORef fvRef
  let timeDiff = currentTime - lastTime
  let newFv = if (timeDiff >= oneSecond)
                then (FramerateValues currentTime 0 (fromIntegral count))
                else (FramerateValues lastTime (count + 1) lastFps)
  writeIORef fvRef newFv
  return $ fvLastFps newFv
  where
    oneSecond = 1000 :: TimeMs
