module Main where

import qualified Graphics.UI.GLFW as GLFW
import Data.IORef

import GLFWWindow
import GLFWInput
import GameLoop
import Draw
import Update

pressedWindowX :: CloseRequested s UserInput
pressedWindowX _ input@(UserInput (WindowInput winClosed _ _) _ _) = winClosed || (isSKeyDown GLFW.ESC input)

main :: IO ()
main = do
  inputRef <- newIORef initialInput
  openWindowWithCfgFile
    defaultWindowConfig
    defaultGLConfig
    inputRef
    "config.cfg"
  gameLoop
    inputRef
    tickTime
    pressedWindowX
    interpolateGameState
    draw
    myGameSystem
  closeWindow
  putStrLn $ "Exiting."
