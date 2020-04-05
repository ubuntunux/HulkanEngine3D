{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}

module HulkanEngine3D.Vulkan.UniformBuffer
    ( UniformBufferDatas (..)
    , UniformBufferData (..)
    , SceneConstantsData (..)
    , createUniformBuffer
    , destroyUniformBuffer
    , defaultUniformBufferDatas
    , createUniformBufferDatas
    , destroyUniformBufferDatas
    ) where


import Control.Monad (replicateM, forM_)
import Data.Bits ((.|.))
import GHC.Generics (Generic)

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Numeric.DataFrame

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan.Buffer
import HulkanEngine3D.Vulkan.Descriptor


data UniformBufferData = UniformBufferData
    { _uniformBuffers :: [VkBuffer]
    , _uniformBufferMemories :: [VkDeviceMemory]
    , _uniformBufferDataSize :: VkDeviceSize
    , _descriptorBufferInfos :: [VkDescriptorBufferInfo]
    } deriving (Eq, Show)

data UniformBufferDatas = UniformBufferDatas
    { _sceneConstantsBufferData :: UniformBufferData
    } deriving (Eq, Show)

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


createUniformBuffer :: VkPhysicalDevice -> VkDevice -> Int -> VkDeviceSize -> IO [(VkDeviceMemory, VkBuffer)]
createUniformBuffer physicalDevice device bufferCount bufferSize = do
    logInfo "createUniformBuffer"
    replicateM bufferCount $ createBuffer
        physicalDevice
        device
        bufferSize
        VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
        ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

destroyUniformBuffer :: VkDevice -> [VkBuffer] -> [VkDeviceMemory] -> IO ()
destroyUniformBuffer device buffers memories = do
    logInfo "destroyUniformBuffers"
    forM_ (zip buffers memories) $ \(buffer, memory) ->
        destroyBuffer device buffer memory

defaultUniformBufferData :: UniformBufferData
defaultUniformBufferData = UniformBufferData
    { _uniformBuffers = []
    , _uniformBufferMemories = []
    , _uniformBufferDataSize = 0
    , _descriptorBufferInfos = []
    }

createUniformBufferData :: VkPhysicalDevice -> VkDevice -> VkDeviceSize -> IO UniformBufferData
createUniformBufferData physicalDevice device bufferSize = do
    (uniformBufferMemories, uniformBuffers) <- unzip <$> createUniformBuffer
        physicalDevice
        device
        Constants.swapChainImageCount
        bufferSize
    let descriptorBufferInfos = map (\uniformBuffer -> createDescriptorBufferInfo uniformBuffer bufferSize) uniformBuffers
    return UniformBufferData
        { _uniformBuffers = uniformBuffers
        , _uniformBufferMemories = uniformBufferMemories
        , _uniformBufferDataSize = bufferSize
        , _descriptorBufferInfos = descriptorBufferInfos
        }

destroyUniformBufferData :: VkDevice -> UniformBufferData -> IO ()
destroyUniformBufferData device uniformBufferData =
    destroyUniformBuffer device (_uniformBuffers uniformBufferData) (_uniformBufferMemories uniformBufferData)


defaultUniformBufferDatas :: UniformBufferDatas
defaultUniformBufferDatas = UniformBufferDatas
    { _sceneConstantsBufferData = defaultUniformBufferData
    }

createUniformBufferDatas :: VkPhysicalDevice -> VkDevice -> IO UniformBufferDatas
createUniformBufferDatas physicalDevice device = do
    sceneConstantsBufferData <- createUniformBufferData physicalDevice device (bSizeOf @SceneConstantsData undefined)
    return UniformBufferDatas
        { _sceneConstantsBufferData = sceneConstantsBufferData
        }

destroyUniformBufferDatas :: VkDevice -> UniformBufferDatas -> IO ()
destroyUniformBufferDatas device uniformBufferDatas = do
    destroyUniformBufferData device (_sceneConstantsBufferData uniformBufferDatas)