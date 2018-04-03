import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  putStrLn "shutdown"
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = do
  putStrLn $ "resizeWindow " ++ (show w) ++ " " ++ (show h)
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

onDisplay :: Window -> IO ()
onDisplay win = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  GLFW.swapBuffers win

  forever $ do
    GLFW.pollEvents
    onDisplay win

main :: IO ()
main = do
  GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 640 480 "Haskell OpenGL" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setWindowSizeCallback win (Just resizeWindow)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)
  -- descriptor <- initResources
  onDisplay win
  shutdown win
