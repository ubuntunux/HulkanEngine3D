{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan.Sync
  ( createSemaphores
  , destroySemaphores
  , createFrameFences
  , destroyFrameFences
  ) where

import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import qualified Library.Constants as Constants
import Library.Utilities.System
import Library.Utilities.Logger


createSemaphores :: VkDevice -> IO [VkSemaphore]
createSemaphores device = do
  semaphores <- allocaArray Constants.maxFrameCount $ \semaphoresPtr -> do
    let semaphoreCreateInfo = createVk @VkSemaphoreCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"flags" VK_ZERO_FLAGS
    forM_ [0..(Constants.maxFrameCount - 1)] $ \index -> do
      withPtr semaphoreCreateInfo $ \semaphoreCreateInfoPtr -> do
          result <- vkCreateSemaphore device semaphoreCreateInfoPtr VK_NULL (ptrAtIndex semaphoresPtr index)
          validationVK result "vkCreateSemaphore failed!"
    peekArray Constants.maxFrameCount semaphoresPtr
  logInfo $ "Create Semaphore: " ++ show semaphores
  return semaphores

destroySemaphores :: VkDevice -> [VkSemaphore] -> IO ()
destroySemaphores device semaphores = do
  forM_ semaphores $ \semaphore ->
    vkDestroySemaphore device semaphore VK_NULL
  logInfo $ "Destroy Semaphore: " ++ show semaphores

createFrameFences :: VkDevice -> IO (Ptr VkFence)
createFrameFences device = do
  frameFencesPtr <- mallocArray Constants.maxFrameCount
  let fenceCreateInfo = createVk @VkFenceCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_FENCE_CREATE_SIGNALED_BIT
  forM_ [0..(Constants.maxFrameCount - 1)] $ \index -> do
    withPtr fenceCreateInfo $ \fenceCreateInfoPtr -> do
        result <- vkCreateFence device fenceCreateInfoPtr VK_NULL (ptrAtIndex frameFencesPtr index)
        validationVK result "vkCreateSemaphore failed!"
  fences <- peekArray Constants.maxFrameCount frameFencesPtr
  logInfo $ "Create VkFences: " ++ show fences
  return frameFencesPtr

destroyFrameFences :: VkDevice -> Ptr VkFence -> IO ()
destroyFrameFences device frameFencesPtr = do
  fences <- peekArray Constants.maxFrameCount frameFencesPtr
  logInfo $ "Destroy VkFence: " ++ show fences
  forM_ fences $ \fence ->
    vkDestroyFence device fence VK_NULL
  free frameFencesPtr