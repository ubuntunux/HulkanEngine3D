{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan.Buffer
  ( createBuffer
  , destroyBuffer
  , copyBuffer
  , findMemoryType
  ) where

import Data.Bits
import Numeric.DataFrame
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame

import Library.Utils
import Library.Logger
import Library.Vulkan


createBuffer :: VkPhysicalDevice
             -> VkDevice
             -> VkDeviceSize
             -> VkBufferUsageFlags
             -> VkMemoryPropertyFlags
             -> IO (VkDeviceMemory, VkBuffer)
createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags = do    
    let bufferCreateInfo = createVk @VkBufferCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"size" bufferSize
          &* set @"usage" bufferUsageFlags
          &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
          &* set @"queueFamilyIndexCount" 0
          &* set @"pQueueFamilyIndices" VK_NULL

    -- create buffer
    buffer <- allocaPeek $ \bufferPtr -> do
      withPtr bufferCreateInfo $ \createInfoPtr -> do
        result <- vkCreateBuffer device createInfoPtr VK_NULL bufferPtr
        validationVK result "vkCreateBuffer failed!"

    memoryRequirements <- allocaPeek $ vkGetBufferMemoryRequirements device buffer
    memoryTypeIndex <- findMemoryType physicalDevice (getField @"memoryTypeBits" memoryRequirements) memoryPropertyFlags
    
    let allocInfo = createVk @VkMemoryAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"allocationSize" (getField @"size" memoryRequirements)
          &* set @"memoryTypeIndex" memoryTypeIndex
          
    -- create allocate memory
    bufferMemory <- allocaPeek $ \bufferMemoryPtr -> do
      withPtr allocInfo $ \allocInfoPtr -> do
        result <- vkAllocateMemory device allocInfoPtr VK_NULL bufferMemoryPtr
        validationVK result "vkAllocateMemory failed!"

    logInfo $ "Create Buffer : "  ++ show buffer ++ ", Memory : " ++ show bufferMemory
    logInfo $ "    bufferSize : " ++ show bufferSize
    logInfo $ "    memoryTypeIndex : " ++ show memoryTypeIndex
    logInfo $ "    " ++ show memoryRequirements

    let memoryOffset = 0 :: VkDeviceSize
    vkBindBufferMemory device buffer bufferMemory memoryOffset

    return (bufferMemory, buffer)

destroyBuffer :: VkDevice -> VkBuffer -> VkDeviceMemory -> IO ()
destroyBuffer device buffer memory = do
  logInfo $ "Destroy Buffer : "  ++ show buffer ++ ", Memory : " ++ show memory
  vkDestroyBuffer device buffer VK_NULL
  vkFreeMemory device memory VK_NULL


-- | @copyBuffer dev pool queue src dest n@ copies @n@ bytes from @src@ buffer to @dest@ buffer.
copyBuffer :: VkDevice
           -> VkCommandPool
           -> VkQueue
           -> VkBuffer 
           -> VkBuffer 
           -> VkDeviceSize 
           -> IO ()
copyBuffer device commandPool commandQueue srcBuffer dstBuffer bufferSize = do
  logInfo $ "CopyBuffer : " ++ show srcBuffer ++ " -> " ++ show dstBuffer ++ " { size = " ++ show bufferSize ++ " }"
  runCommandsOnce device commandPool commandQueue $ \commandBuffer -> do
    let copyRegion = createVk @VkBufferCopy
          $  set @"srcOffset" 0
          &* set @"dstOffset" 0
          &* set @"size" bufferSize
    withPtr copyRegion $ \copyRegionPtr -> do
      vkCmdCopyBuffer commandBuffer srcBuffer dstBuffer 1 copyRegionPtr


-- | Return an index of a memory type for a device
findMemoryType :: VkPhysicalDevice
               -> Word32 -- ^ type filter bitfield
               -> VkMemoryPropertyFlags -- ^ desired memory properties                  
               -> IO Word32
findMemoryType physicalDevice typeFilter propertyFlags = do
  memProps <- allocaPeek $ \ptr -> vkGetPhysicalDeviceMemoryProperties physicalDevice ptr
  let mtCount = getField @"memoryTypeCount" memProps
      memTypes = getVec @"memoryTypes" memProps
      flags index = getField @"propertyFlags" (ixOff (fromIntegral index) memTypes)
      go i | i == mtCount = return i
           | otherwise = if testBit typeFilter (fromIntegral i) && 
                            (propertyFlags == (flags i .&. propertyFlags))
                         then return i
                         else go (i + 1)
  go 0
