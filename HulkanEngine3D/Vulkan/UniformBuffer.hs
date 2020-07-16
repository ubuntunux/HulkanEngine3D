{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}

module HulkanEngine3D.Vulkan.UniformBuffer
    ( UniformBufferData (..)
    , defaultUniformBufferData
    , createUniformBufferData
    , destroyUniformBufferData
    ) where


import Control.Monad (replicateM, forM_)
import Data.Bits ((.|.))
import qualified Data.Text as Text

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Vulkan.Buffer
import HulkanEngine3D.Vulkan.Descriptor


data UniformBufferData = UniformBufferData
    { _uniformBufferName :: Text.Text
    , _uniformBuffers :: [VkBuffer]
    , _uniformBufferMemories :: [VkDeviceMemory]
    , _uniformBufferDataSize :: VkDeviceSize
    , _descriptorBufferInfos :: [VkDescriptorBufferInfo]
    } deriving (Eq, Show)

defaultUniformBufferData :: UniformBufferData
defaultUniformBufferData = UniformBufferData
    { _uniformBufferName = ""
    , _uniformBuffers = []
    , _uniformBufferMemories = []
    , _uniformBufferDataSize = 0
    , _descriptorBufferInfos = []
    }

createUniformBuffer :: VkPhysicalDevice -> VkDevice -> Text.Text -> Int -> VkDeviceSize -> IO [(VkDeviceMemory, VkBuffer)]
createUniformBuffer physicalDevice device uniformBufferName bufferCount bufferSize = do
    logInfo $ "createUniformBuffer : " ++ Text.unpack uniformBufferName
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

createUniformBufferData :: VkPhysicalDevice -> VkDevice -> Text.Text -> VkDeviceSize -> IO UniformBufferData
createUniformBufferData physicalDevice device uniformBufferName bufferSize = do
    (uniformBufferMemories, uniformBuffers) <- unzip <$> createUniformBuffer
        physicalDevice
        device
        uniformBufferName
        Constants.swapChainImageCount
        bufferSize
    let descriptorBufferInfos = map (\uniformBuffer -> createDescriptorBufferInfo uniformBuffer bufferSize) uniformBuffers
    return UniformBufferData
        { _uniformBufferName = uniformBufferName
        , _uniformBuffers = uniformBuffers
        , _uniformBufferMemories = uniformBufferMemories
        , _uniformBufferDataSize = bufferSize
        , _descriptorBufferInfos = descriptorBufferInfos
        }

destroyUniformBufferData :: VkDevice -> UniformBufferData -> IO ()
destroyUniformBufferData device uniformBufferData =
    destroyUniformBuffer device (_uniformBuffers uniformBufferData) (_uniformBufferMemories uniformBufferData)
