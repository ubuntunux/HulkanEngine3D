{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}

module HulkanEngine3D.Vulkan.UniformBuffer
    ( UniformBufferData (..)
    , defaultUniformBufferData
    , createUniformBufferData
    , destroyUniformBufferData
    ) where


import Control.Monad (replicateM, forM_)
import Data.Bits ((.|.))

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

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

defaultUniformBufferData :: UniformBufferData
defaultUniformBufferData = UniformBufferData
    { _uniformBuffers = []
    , _uniformBufferMemories = []
    , _uniformBufferDataSize = 0
    , _descriptorBufferInfos = []
    }

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
