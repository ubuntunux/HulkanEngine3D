{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE Strict             #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Vulkan.FrameBuffer
    ( FrameBufferData (..)
    , FrameBufferDataCreateInfo
    , defaultFrameBufferDataCreateInfo
    , createFrameBufferData
    , destroyFrameBufferData
    ) where

import qualified Data.Text as Text
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Storable

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger


data FrameBufferData = FrameBufferData
    { _frameBufferName :: Text.Text
    , _frameBufferWidth :: Int
    , _frameBufferHeight :: Int
    , _frameBufferDepth :: Int
    , _frameBufferViewPort :: VkViewport
    , _frameBufferScissorRect :: VkRect2D
    , _frameBufferImageViewsList :: [[VkImageView]]
    , _frameBufferClearValues :: [VkClearValue]
    , _frameBuffers :: [VkFramebuffer]
    , _renderPassBeginInfos :: [VkRenderPassBeginInfo]
    }  deriving (Eq, Show)

type FrameBufferDataCreateInfo = FrameBufferData

defaultFrameBufferDataCreateInfo :: FrameBufferDataCreateInfo
defaultFrameBufferDataCreateInfo = FrameBufferData
    { _frameBufferName = ""
    , _frameBufferWidth = 0
    , _frameBufferHeight = 0
    , _frameBufferDepth = 1
    , _frameBufferViewPort = createViewport 0 0 0 0 0 0
    , _frameBufferScissorRect = createScissorRect 0 0 0 0
    , _frameBufferImageViewsList = []
    , _frameBufferClearValues = []
    , _frameBuffers = []
    , _renderPassBeginInfos = []
    }


createFrameBufferData :: VkDevice
                      -> VkRenderPass
                      -> FrameBufferDataCreateInfo
                      -> IO FrameBufferData
createFrameBufferData device renderPass frameBufferDataCreateInfo = do
    logInfo $ "Create Framebuffers : " ++ (Text.unpack $ _frameBufferName frameBufferDataCreateInfo)
    logInfo $ "    ImageSize " ++ show
        ( _frameBufferWidth $ frameBufferDataCreateInfo
        , _frameBufferHeight $ frameBufferDataCreateInfo
        , _frameBufferDepth $ frameBufferDataCreateInfo
        )

    frameBuffers <- mapM createFrameBuffer (_frameBufferImageViewsList frameBufferDataCreateInfo)

    let renderPassBeginInfo frameBuffer = createVk @VkRenderPassBeginInfo
            $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
            &* set @"pNext" VK_NULL
            &* set @"renderPass" renderPass
            &* set @"framebuffer" frameBuffer
            &* setVk @"renderArea"
                (  setVk @"offset"
                        (  set @"x" 0
                        &* set @"y" 0
                        )
                &* setVk @"extent"
                    (  set @"width" (fromIntegral $ _frameBufferWidth frameBufferDataCreateInfo)
                    &* set @"height" (fromIntegral $ _frameBufferHeight frameBufferDataCreateInfo)
                    )
                )
            &* set @"clearValueCount" (fromIntegral . length $ _frameBufferClearValues frameBufferDataCreateInfo)
            &* setListRef @"pClearValues" (_frameBufferClearValues frameBufferDataCreateInfo)
        renderPassBeginInfos = map renderPassBeginInfo frameBuffers

    return frameBufferDataCreateInfo
        { _frameBuffers = frameBuffers
        , _renderPassBeginInfos = renderPassBeginInfos
        }
    where
        createFrameBuffer :: [VkImageView] -> IO VkFramebuffer
        createFrameBuffer imageViews = do
            let frameBufferCreateInfo = createVk @VkFramebufferCreateInfo
                    $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                    &* set @"pNext" VK_NULL
                    &* set @"flags" VK_ZERO_FLAGS
                    &* set @"renderPass" renderPass
                    &* set @"attachmentCount" (fromIntegral $ length imageViews)
                    &* setListRef @"pAttachments" imageViews
                    &* set @"width" (fromIntegral . _frameBufferWidth $ frameBufferDataCreateInfo)
                    &* set @"height" (fromIntegral . _frameBufferHeight $ frameBufferDataCreateInfo)
                    &* set @"layers" (fromIntegral . _frameBufferDepth $ frameBufferDataCreateInfo)
            frameBuffer <- alloca $ \framebufferPtr -> do
                result <- vkCreateFramebuffer device (unsafePtr frameBufferCreateInfo) VK_NULL framebufferPtr
                validationVK result "vkCreateFramebuffer failed!"
                peek framebufferPtr
            touchVkData frameBufferCreateInfo
            return frameBuffer


destroyFrameBufferData :: VkDevice -> FrameBufferData -> IO ()
destroyFrameBufferData device frameBufferData = do
    logInfo $ "Destroy Framebuffers : " ++ show (_frameBufferName frameBufferData) ++ " "  ++ show (_frameBuffers frameBufferData)
    forM_ (_frameBuffers frameBufferData) $ \frameBuffer ->
        vkDestroyFramebuffer device frameBuffer VK_NULL_HANDLE