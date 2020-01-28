{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan.FrameBuffer
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

import Library.Utils
import Library.Logger

data FrameBufferData = FrameBufferData
    { _frameBuffers :: [VkFramebuffer]
    , _frameBufferSize :: VkExtent2D
    , _frameBufferClearValues :: [Float]
    } deriving (Eq, Show)


createFramebufferData :: VkDevice
                      -> VkRenderPass
                      -> [VkImageView]
                      -> VkExtent2D
                      -> [Float]
                      -> IO FrameBufferData
createFramebufferData device renderPass imageViews imageExtent clearValues = do
    framebuffers <- mapM createFrameBuffer imageViews
    logInfo $ "Create Framebuffers: " ++ show framebuffers
    return FrameBufferData
        { _frameBuffers = framebuffers
        , _frameBufferSize = imageExtent
        , _frameBufferClearValues = clearValues }
    where
        createFrameBuffer :: VkImageView -> IO VkFramebuffer
        createFrameBuffer imageView = do
            let frameBufferCreateInfo = createVk @VkFramebufferCreateInfo
                    $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                    &* set @"pNext" VK_NULL
                    &* set @"flags" VK_ZERO_FLAGS
                    &* set @"renderPass" renderPass
                    &* set @"attachmentCount" 1
                    &* setListRef @"pAttachments" [imageView]
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
  logInfo $ "Destroy Framebuffers" ++ show frameBufferData
  forM_ (_frameBuffers frameBufferData) $ \frameBuffer ->
    vkDestroyFramebuffer device frameBuffer VK_NULL_HANDLE