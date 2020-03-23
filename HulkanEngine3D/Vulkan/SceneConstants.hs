{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}

module HulkanEngine3D.Vulkan.SceneConstants
  ( SceneConstantsData (..)
  , createSceneConstantsBuffers
  , destroySceneConstantsBuffers
  , sceneConstantsBufferSize
  ) where


import Control.Monad (replicateM, forM_)
import Data.Bits ((.|.))
import GHC.Generics (Generic)

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Numeric.DataFrame

import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan.Buffer

{- |
  Here, in the `view` matrix, we have to decide on our world coordinates.
  This should always be a right-triple, which is forced by lookAt matrix and
  many others. So, here are popular options
    right  up   forward
     x     y      -z     <-- classical OpenGL approach, coincides with projection space
     x     z       y     <-- all positive, z being "up" is kind of intuitive
     ... and a few others, e.g.
     y     z      -x

  I stick x-z-y, because the same triple is used in chalet.obj from the tutorial.
  This affects the choice of axis rotation and lookAt up vector below.

  More about these transformtions can be found be the link:
  https://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
 -}
data SceneConstantsData = SceneConstantsData
  { _VIEW  :: Mat44f
  , _PROJECTION  :: Mat44f
  } deriving (Show, Generic)

instance PrimBytes SceneConstantsData


createSceneConstantsBuffers :: VkPhysicalDevice -> VkDevice -> Int -> IO [(VkDeviceMemory, VkBuffer)]
createSceneConstantsBuffers physicalDevice device bufferCount = do
    logInfo "createSceneConstantsBuffer"
    replicateM bufferCount $ createBuffer
        physicalDevice
        device
        (bSizeOf @SceneConstantsData undefined)
        VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
        ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

destroySceneConstantsBuffers :: VkDevice -> [VkBuffer] -> [VkDeviceMemory] -> IO ()
destroySceneConstantsBuffers device buffers memories = do
    logInfo "destroySceneConstantsBuffers"
    forM_ (zip buffers memories) $ \(buffer, memory) ->
        destroyBuffer device buffer memory


sceneConstantsBufferSize :: VkDeviceSize
sceneConstantsBufferSize = bSizeOf @SceneConstantsData undefined