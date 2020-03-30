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
                      -> (Int, Int, Int)
                      -> VkSampleCountFlagBits
                      -> [VkClearValue]
                      -> IO FrameBufferData
createFramebufferData device renderPass swapChainImageCount imageViews (width, height, depth) msaaSampleCount clearValues = do
    logInfo "Create Framebuffers"
    logInfo $ "    ImageViews " ++ show imageViews
    when (msaaSampleCount /= VK_SAMPLE_COUNT_1_BIT) $ do
        logInfo $ "    MSAA " ++ show msaaSampleCount
    framebuffers <- mapM createFrameBuffer [0..(swapChainImageCount - 1)]
    let imageExtent = createVk @VkExtent2D
            $  set @"width" (fromIntegral width)
            &* set @"height" (fromIntegral height)
    return FrameBufferData
        { _frameBuffers = framebuffers
        , _frameBufferSize = imageExtent
        , _frameBufferClearValues = clearValues }
    where
        createFrameBuffer :: Int -> IO VkFramebuffer
        createFrameBuffer index = do
            let frameBufferCreateInfo = createVk @VkFramebufferCreateInfo
                    $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                    &* set @"pNext" VK_NULL
                    &* set @"flags" VK_ZERO_FLAGS
                    &* set @"renderPass" renderPass
                    &* set @"attachmentCount" (fromIntegral $ length imageViews)
                    &* setListRef @"pAttachments" imageViews
                    &* set @"width" (fromIntegral width)
                    &* set @"height" (fromIntegral height)
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