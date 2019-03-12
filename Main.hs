module Main (main) where

import Control.Exception
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.UI.GLFW (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Lib.Utils
import Lib.Vulkan (withVulkanInstance)


someAcion window = do
  print window
  return ()

main::IO()
main = do 
  window <- withGLFWWindow $ someAcion
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()


withGLFWWindow action = do
  GLFW.init >>= (\result -> flip unless (throwVKMsg "Failed to initialize GLFW.") result)
  putStrLn "Initialized GLFW."
  GLFW.getVersionString >>= (\result -> mapM_ (putStrLn . ("GLFW Version: " ++)) result)
  GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable True
  window <- GLFW.createWindow 800 600 "Vulkan Window" Nothing Nothing    
  case window of
    Nothing -> throwVKMsg "Failed to initialize GLFW window."
    Just w -> do
      putStrLn "Initialized GLFW window."
      finally (action w)
        (GLFW.destroyWindow w >> putStrLn "Closed GLFW window.")
    

