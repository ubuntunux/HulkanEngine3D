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


--createVulkanInstance::String -> String -> [CString] -> [String] -> IO VkInstance
createVulkanInstance progName engineName extensions layers = do
 return ()


someAcion window = do
  glfwReqExts <- GLFW.getRequiredInstanceExtensions
  let
    progName = "02-GLFWWindow"
    engineName = "My perfect Haskell engine"
    extensions = glfwReqExts
    layers = ["VK_LAYER_LUNARG_standard_validation"]
  vkInstance <- createVulkanInstance progName engineName extensions layers
  print vkInstance
  return ()

main::IO()
main = do 
  maybeWindow <- withGLFWWindow
  when (Nothing == maybeWindow) (throwVKMsg "Failed to initialize GLFW window.")
  putStrLn "Initialized GLFW window."
  let Just window = maybeWindow  
  someAcion window
  GLFW.destroyWindow window >> putStrLn "Closed GLFW window."
  GLFW.terminate >> putStrLn "Terminated GLFW."
  return ()

withGLFWWindow::IO (Maybe GLFW.Window)
withGLFWWindow = do
  GLFW.init >>= flip unless (throwVKMsg "Failed to initialize GLFW.")
  putStrLn "Initialized GLFW."
  version <- GLFW.getVersionString
  mapM_ (putStrLn . ("GLFW Version: " ++)) version
  GLFW.vulkanSupported >>= flip unless (throwVKMsg "GLFW reports that vulkan is not supported!")
  GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  GLFW.windowHint $ WindowHint'Resizable True
  window <- GLFW.createWindow 800 600 "Vulkan Window" Nothing Nothing
  return window
        
    

