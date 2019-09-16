{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan.Buffer
  ( createBuffer
  , destroyBuffer
--   , copyBuffer
--   , findMemoryType
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


destroyBuffer :: VkDevice -> VkBuffer -> IO ()
destroyBuffer device buffer = do
  vkDestroyBuffer device buffer VK_NULL

createBuffer :: VkPhysicalDevice
             -> VkDevice
             -> VkDeviceSize
             -> VkBufferUsageFlags
             -> VkMemoryPropertyFlags
             -> IO ()
            --  -> IO (VkDeviceMemory, VkBuffer)
createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags = do    
    let bufferCreateInfo = createVk @VkBufferCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"size" bufferSize
          &* set @"usage" bufferUsageFlags
          &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
          &* set @"queueFamilyIndexCount" 0
          &* set @"pQueueFamilyIndices" VK_NULL
    buffer <- alloca $ \bufferPtr -> do
      withPtr bufferCreateInfo $ \createInfoPtr -> do
        throwingVK "bufferCreateInfo failed!"
          $ vkCreateBuffer device createInfoPtr VK_NULL bufferPtr
      peek bufferPtr
    touchVkData bufferCreateInfo

    memRequirements <- alloca $ \bufferPtr -> do
      vkGetBufferMemoryRequirements device buffer bufferPtr
      peek bufferPtr

    putStrLn $ "createBuffer : " ++ show memRequirements
    
    return ()
--     (buf, freeBufLater) <- allocResource'
--       (\vb -> liftIO $ vkDestroyBuffer dev vb VK_NULL) $
--       withVkPtr bufferInfo $ \biPtr -> allocaPeek $
--         runVk . vkCreateBuffer dev biPtr VK_NULL

--     -- find its memory requirements
--     memRequirements <- allocaPeek $
--       liftIO . vkGetBufferMemoryRequirements dev buf

--     memIndex <- findMemoryType pdev (getField @"memoryTypeBits" memRequirements)
--                                     bMemPropFlags

--     -- allocate memory
--     let allocInfo = createVk @VkMemoryAllocateInfo
--           $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
--           &* set @"pNext" VK_NULL
--           &* set @"allocationSize" (getField @"size" memRequirements)
--           &* set @"memoryTypeIndex" memIndex

--     bufferMemory <- allocResource
--       (\vbm -> liftIO $ vkFreeMemory dev vbm VK_NULL) $
--       withVkPtr allocInfo $ \aiPtr -> allocaPeek $
--         runVk . vkAllocateMemory dev aiPtr VK_NULL
--     -- The buf will be released before release of any of the resources
--     -- allocated above, but after release on any allocations below.
--     freeBufLater

--     -- associate memory with buffer
--     runVk $ vkBindBufferMemory dev buf bufferMemory 0

--     return (bufferMemory, buf)

-- -- | @copyBuffer dev pool queue src dest n@ copies @n@ bytes from @src@ buffer
-- --   to @dest@ buffer.
-- copyBuffer :: VkDevice
--            -> VkCommandPool
--            -> VkQueue
--            -> VkBuffer -> VkBuffer -> VkDeviceSize -> Program r ()
-- copyBuffer dev commandPool cmdQueue srcBuffer dstBuffer bSize =
--   runCommandsOnce dev commandPool cmdQueue $ \cmdBuf -> do
--     let copyRegion = createVk @VkBufferCopy
--           $  set @"srcOffset" 0
--           &* set @"dstOffset" 0
--           &* set @"size" bSize
--     withVkPtr copyRegion $ liftIO . vkCmdCopyBuffer cmdBuf srcBuffer dstBuffer 1


-- -- | Return an index of a memory type for a device
-- findMemoryType :: VkPhysicalDevice
--                -> Word32 -- ^ type filter bitfield
--                -> VkMemoryPropertyFlags
--                   -- ^ desired memory properties
--                -> Program r Word32
-- findMemoryType pdev typeFilter properties = do
--     memProps <- allocaPeek $ liftIO . vkGetPhysicalDeviceMemoryProperties pdev
--     let mtCount = getField @"memoryTypeCount" memProps
--         memTypes = getVec @"memoryTypes" memProps
--         go i | i == mtCount = throwVkMsg "Failed to find suitable memory type!"
--              | otherwise = if    testBit typeFilter (fromIntegral i)
--                               && ( getField @"propertyFlags"
--                                         (ixOff (fromIntegral i) memTypes)
--                                     .&. properties
--                                  ) == properties
--                            then return i
--                            else go (i+1)
--     go 0

createVertexBuffer :: RenderData
                   -> DataFrame Vertex '[XN 3] -- ^ A collection of at least three vertices
                   -> IO ()
                   -- -> IO VkBuffer
createVertexBuffer renderData (XFrame vertices) = do
  let 
      device = _device renderData
      physicalDevice = _physicalDevice renderData
      graphicsQueue = _graphicsQueue $ _queueFamilyDatas renderData
      commandPool = _commandPool renderData
      bufferSize = bSizeOf vertices
      bufferUsageFlags = ( VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT )
      memoryPropertyFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  buffer <- createBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags
  return ()
--     let bSize = bSizeOf vertices

--     (_, vertexBuf) <-
--       createBuffer pdev dev bSize
--         ( VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT )
--         VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

--     -- Use "locally" to destroy temporary staging buffer after data copy is complete
--     locally $ do
--       (stagingMem, stagingBuf) <-
--         createBuffer pdev dev bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT
--           ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

--       -- copy data
--       stagingDataPtr <- allocaPeek $
--         runVk . vkMapMemory dev stagingMem 0 bSize VK_ZERO_FLAGS
--       poke (castPtr stagingDataPtr) vertices
--       liftIO $ vkUnmapMemory dev stagingMem
--       copyBuffer dev cmdPool cmdQueue stagingBuf vertexBuf bSize

--     return vertexBuf


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
