module GLFWWindow where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Data.IORef
import Data.Configurator
import Data.Configurator.Types
import Data.Text

import GLFWInput

data WindowConfig = WindowConfig {
  winWidth :: Int,
  winHeight :: Int,
  winTitle :: String
}
defaultWindowConfig :: WindowConfig
defaultWindowConfig = WindowConfig 800 600 "GLFW Window"

data GLConfig = GLConfig {
  glConfBlend :: Capability,
  glConfBlendFunc :: (BlendingFactor, BlendingFactor),
  glConfClearColor :: Color4 GLclampf,
  glConfTexture2D :: Capability
}
defaultGLConfig :: GLConfig
defaultGLConfig = GLConfig GL.Enabled (GL.SrcAlpha, GL.OneMinusSrcAlpha) (Color4 0 0 0 0) GL.Enabled

-- Initialize GLFW window and OpenGL Context
openWindow :: WindowConfig -> GLConfig -> IORef UserInput -> Config -> IO UserInput
openWindow winConf glConf inputRef conf = do
  let windowConfig = subconfig (pack "window") conf
  windowWidth <- lookupDefault (winWidth winConf) windowConfig (pack "width") :: IO Int
  windowHeight <- lookupDefault (winHeight winConf) windowConfig (pack "height") :: IO Int
  let windowSize = (GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight))

  GLFW.initialize
  GLFW.disableSpecial GLFW.AutoPollEvent
  True <- GLFW.openWindow windowSize [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle  $= (winTitle winConf)

  GL.blend      $= glConfBlend glConf
  GL.blendFunc  $= glConfBlendFunc glConf
  GL.clearColor $= glConfClearColor glConf

  GL.texture Texture2D $= glConfTexture2D glConf

  -- So that the input is going to be saved inside 'inputRef'
  registerInputCallbacks inputRef

  return initialInput

closeWindow :: IO ()
closeWindow = GLFW.closeWindow >> GLFW.terminate

loadConfig :: FilePath -> IO Config
loadConfig configPath = do
  let files = [Required configPath]
  load files

openWindowWithCfgFile :: WindowConfig -> GLConfig -> IORef UserInput -> FilePath -> IO UserInput
openWindowWithCfgFile wc glc i configPath = (loadConfig configPath) >>= (openWindow wc glc i)
