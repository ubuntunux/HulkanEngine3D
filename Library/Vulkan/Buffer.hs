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
  , createVertexBuffer
--   , createIndexBuffer
  ) where

import Data.Bits
import Foreign.Ptr              (castPtr)
import Foreign.Marshal.Alloc
import Foreign.Storable
import Numeric.DataFrame
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Create.DataFrame

import Library.Utils
import Library.Vulkan
import Library.Vulkan.Mesh


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
        throwingVK "vkCreateBuffer failed!"
          $ vkCreateBuffer device createInfoPtr VK_NULL bufferPtr

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
        throwingVK "vkAllocateMemory failed!"
          $ vkAllocateMemory device allocInfoPtr VK_NULL bufferMemoryPtr

    putStrLn $ "Create Buffer : "  ++ show buffer ++ ", Memory : " ++ show bufferMemory
    putStrLn $ "\tbufferSize : " ++ show bufferSize
    putStrLn $ "\tmemoryTypeIndex : " ++ show memoryTypeIndex
    putStrLn $ "\t" ++ show memoryRequirements

    let memoryOffset = 0 :: VkDeviceSize
    vkBindBufferMemory device buffer bufferMemory memoryOffset

    return (bufferMemory, buffer)

destroyBuffer :: VkDevice -> VkBuffer -> VkDeviceMemory -> IO ()
destroyBuffer device buffer memory = do
  putStrLn $ "Destroy Buffer : "  ++ show buffer ++ ", Memory : " ++ show memory
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
  putStrLn $ "CopyBuffer : " ++ show srcBuffer ++ " -> " ++ show dstBuffer ++ " { size = " ++ show bufferSize ++ " }"
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


createVertexBuffer :: RenderData
                   -> DataFrame Vertex '[XN 3] -- ^ A collection of at least three vertices
                   -> IO VkBuffer
createVertexBuffer renderData (XFrame vertices) = do
  let device = _device renderData
      physicalDevice = _physicalDevice renderData
      graphicsQueue = _graphicsQueue $ _queueFamilyDatas renderData
      commandPool = _commandPool renderData
      bufferSize = bSizeOf vertices
      bufferUsageFlags = (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
      memoryPropertyFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  -- create vertex buffer
  (vertexMemory, vertexBuffer) <- createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags

  let stagingBufferUsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT
      stagingMemoryPropertyFlags = (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

  -- Destroy temporary staging buffer after data copy is complete.
  (stagingMemory, stagingBuffer) <- createBuffer physicalDevice device bufferSize stagingBufferUsageFlags stagingMemoryPropertyFlags

  -- copy data
  stagingDataPtr <- allocaPeek $ vkMapMemory device stagingMemory 0 bufferSize VK_ZERO_FLAGS
  poke (castPtr stagingDataPtr) vertices
  vkUnmapMemory device stagingMemory
  copyBuffer device commandPool graphicsQueue stagingBuffer vertexBuffer bufferSize

  destroyBuffer device stagingBuffer stagingMemory
  -- destroyBuffer device vertexBuffer vertexMemory
 
  return vertexBuffer

-- createIndexBuffer :: VkPhysicalDevice
--                   -> VkDevice
--                   -> VkCommandPool
--                   -> VkQueue
--                   -> DataFrame Word32 '[XN 3]
--                      -- ^ A collection of at least three indices
--                   -> Program r VkBuffer
-- createIndexBuffer pdev dev cmdPool cmdQueue (XFrame indices) = do

--     let bSize = bSizeOf indices

--     (_, vertexBuf) <-
--       createBuffer pdev dev bSize
--         ( VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_INDEX_BUFFER_BIT )
--         VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

--     -- Use "locally" to destroy temporary staging buffer after data copy is complete
--     locally $ do
--       (stagingMem, stagingBuf) <-
--         createBuffer pdev dev bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT
--           ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

--       -- copy data
--       stagingDataPtr <- allocaPeek $
--         runVk . vkMapMemory dev stagingMem 0 bSize VK_ZERO_FLAGS
--       poke (castPtr stagingDataPtr) indices
--       liftIO $ vkUnmapMemory dev stagingMem
--       copyBuffer dev cmdPool cmdQueue stagingBuf vertexBuf bSize

--     return vertexBuf
