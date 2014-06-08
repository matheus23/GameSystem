module GLFWInput where

import Data.IORef
import Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Data.Vec

data UserInput = UserInput {
  window :: WindowInput,
  mouse :: MouseInput,
  keys :: KeyInput
}
initialInput :: UserInput
initialInput = UserInput initialWindowInput initialMouseInput initialKeyInput

data WindowInput = WindowInput {
  closed :: Bool,
  width :: Int,
  height :: Int
}
initialWindowInput :: WindowInput
initialWindowInput = WindowInput { closed = False, width = 0, height = 0 }

data MouseInput = MouseInput {
  x :: Int,
  y :: Int
}
initialMouseInput :: MouseInput
initialMouseInput = MouseInput { x = 0, y = 0 }

data KeyInput = KeyInput {
  down :: Set GLFW.Key
}
initialKeyInput :: KeyInput
initialKeyInput = KeyInput { down = Set.empty }

-- Callbacks:

resize :: IORef UserInput -> GL.Size -> IO ()
resize inputRef size@(GL.Size w h) =
  do
    GL.viewport   $= (GL.Position 0 0, size)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
    input <- readIORef inputRef
    writeIORef inputRef
      (input {
         window = (window input) {
           width = floor (realToFrac w),
           height = floor (realToFrac h) } })

-- called, when user clicks the "X" On the window
close :: IORef UserInput -> IO Bool
close inputRef = do
  input <- readIORef inputRef
  writeIORef inputRef (input { window = (window input) { closed = True }})
  return False

mousePos :: IORef UserInput -> Position -> IO ()
mousePos inputRef (GL.Position x y) = do
  input <- readIORef inputRef
  writeIORef inputRef (input { mouse = MouseInput (fromIntegral x) (fromIntegral y) })

key :: IORef UserInput -> GLFW.Key -> GLFW.KeyButtonState -> IO ()
key inputRef key state = do
  input <- readIORef inputRef
  let lastDown = (down . keys) input
  let setAction = case state of
                       GLFW.Press -> Set.insert
                       GLFW.Release -> Set.delete
  let newDown = setAction key lastDown
  writeIORef inputRef (input { keys = (keys input) { down = newDown } })

charKey :: IORef UserInput -> Char -> GLFW.KeyButtonState -> IO ()
charKey inputRef char = key inputRef (GLFW.CharKey char)

-- Callback registration:
registerInputCallbacks :: IORef UserInput -> IO ()
registerInputCallbacks inputRef = do
  GLFW.windowSizeCallback  $= resize inputRef
  GLFW.windowCloseCallback $= close inputRef
  GLFW.mousePosCallback    $= mousePos inputRef
  GLFW.keyCallback         $= key inputRef

isKeyDown :: GLFW.Key -> UserInput -> Bool
isKeyDown key input = Set.member key (down $ keys $ input)

isSKeyDown :: GLFW.SpecialKey -> UserInput -> Bool
isSKeyDown = isKeyDown . GLFW.SpecialKey

isCKeyDown :: Char -> UserInput -> Bool
isCKeyDown = isKeyDown . GLFW.CharKey

getMousePos :: UserInput -> Vec2F
getMousePos (UserInput _ (MouseInput x y) _) = Vec2F (fromIntegral x) (fromIntegral y)
