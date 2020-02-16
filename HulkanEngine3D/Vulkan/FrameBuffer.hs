{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module HulkanEngine3D.Vulkan.FrameBuffer
    ( FrameBufferData (..)
    , createFramebufferData
    , destroyFramebufferData
    ) where

import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger

data FrameBufferData = FrameBufferData
    { _frameBuffers :: [VkFramebuffer]
    , _frameBufferSize :: VkExtent2D
    , _frameBufferClearValues :: [VkClearValue]
    } deriving (Eq, Show)


createFramebufferData :: VkDevice
                      -> VkRenderPass
                      -> Int
                      -> [VkImageView]
                      -> [VkImageView]
                      -> VkExtent2D
                      -> VkSampleCountFlagBits
                      -> [VkClearValue]
                      -> IO FrameBufferData
createFramebufferData device renderPass swapChainImageCount imageViews resolveImageViews imageExtent msaaSampleCount clearValues = do
    logInfo "Create Framebuffers"
    logInfo $ "    ImageViews " ++ show imageViews
    when (msaaSampleCount /= VK_SAMPLE_COUNT_1_BIT) $ do
        logInfo $ "    MSAA " ++ show msaaSampleCount
        logInfo $ "    ResolveImageViews " ++ show resolveImageViews
    framebuffers <- mapM createFrameBuffer [0..(swapChainImageCount - 1)]
    return FrameBufferData
        { _frameBuffers = framebuffers
        , _frameBufferSize = imageExtent
        , _frameBufferClearValues = clearValues }
    where
        createFrameBuffer :: Int -> IO VkFramebuffer
        createFrameBuffer index = do
            let attachements = if msaaSampleCount /= VK_SAMPLE_COUNT_1_BIT
                    then imageViews ++ [resolveImageViews !! index]
                    else imageViews
                frameBufferCreateInfo = createVk @VkFramebufferCreateInfo
                    $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                    &* set @"pNext" VK_NULL
                    &* set @"flags" VK_ZERO_FLAGS
                    &* set @"renderPass" renderPass
                    &* set @"attachmentCount" (fromIntegral $ length attachements)
                    &* setListRef @"pAttachments" attachements
                    &* set @"width" (getField @"width" imageExtent)
                    &* set @"height" (getField @"height" imageExtent)
                    &* set @"layers" 1
            frameBuffer <- alloca $ \framebufferPtr -> do
                result <- vkCreateFramebuffer device (unsafePtr frameBufferCreateInfo) VK_NULL framebufferPtr
                validationVK result "vkCreateFramebuffer failed!"
                peek framebufferPtr
            touchVkData frameBufferCreateInfo
            return frameBuffer


destroyFramebufferData :: VkDevice -> FrameBufferData -> IO ()
destroyFramebufferData device frameBufferData = do
    logInfo $ "Destroy Framebuffers : " ++ show (_frameBuffers frameBufferData)
    forM_ (_frameBuffers frameBufferData) $ \frameBuffer ->
        vkDestroyFramebuffer device frameBuffer VK_NULL_HANDLE