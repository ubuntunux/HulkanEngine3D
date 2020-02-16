{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HulkanEngine3D.Vulkan.Texture
    ( TextureData (..)
    , TextureInterface (..)
    , findDepthFormat
    , findSupportedFormat
    , createTextureSampler
    , destroyTextureSampler
    , createImageView
    , destroyImageView
    , createImage
    , destroyImage
    , copyBufferToImage
    , createDepthImageView
    , createColorImageView
    , createTextureImageView
    , destroyTextureData
    ) where

import Control.Monad
import Codec.Picture
import Data.Bits
import qualified Data.Vector.Storable as Vec
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (castPtr)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Vulkan.Buffer
import {-# SOURCE #-} HulkanEngine3D.Vulkan


data TextureData = TextureData
    { _imageView :: VkImageView
    , _image :: VkImage
    , _imageMemory :: VkDeviceMemory
    , _textureSampler ::VkSampler
    , _imageMipLevels :: Word32 }
    deriving (Eq, Show)


class TextureInterface a where
    getTextureImageInfo :: a -> VkDescriptorImageInfo

instance TextureInterface TextureData where
    getTextureImageInfo textureData =
        createVk @VkDescriptorImageInfo
            $  set @"imageLayout" VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            &* set @"imageView" (_imageView textureData)
            &* set @"sampler" (_textureSampler textureData)


data ImageLayoutTransition = Undef_TransDst | TransDst_ShaderRO | Undef_DepthStencilAtt | Undef_ColorAtt

data TransitionDependent = TransitionDependent
    { _oldLayout     :: VkImageLayout
    , _newLayout     :: VkImageLayout
    , _srcAccessMask :: VkAccessFlags
    , _dstAccessMask :: VkAccessFlags
    , _srcStageMask  :: VkPipelineStageFlags
    , _dstStageMask  :: VkPipelineStageFlags }

transitionDependent :: ImageLayoutTransition -> TransitionDependent
transitionDependent Undef_TransDst = TransitionDependent
    { _oldLayout      = VK_IMAGE_LAYOUT_UNDEFINED
    , _newLayout      = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    , _srcAccessMask  = VK_ZERO_FLAGS
    , _dstAccessMask  = VK_ACCESS_TRANSFER_WRITE_BIT
    , _srcStageMask   = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
    , _dstStageMask   = VK_PIPELINE_STAGE_TRANSFER_BIT }
transitionDependent TransDst_ShaderRO = TransitionDependent
    { _oldLayout      = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    , _newLayout      = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    , _srcAccessMask  = VK_ACCESS_TRANSFER_WRITE_BIT
    , _dstAccessMask  = VK_ACCESS_SHADER_READ_BIT
    , _srcStageMask   = VK_PIPELINE_STAGE_TRANSFER_BIT
    , _dstStageMask   = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT }
transitionDependent Undef_DepthStencilAtt = TransitionDependent
    { _oldLayout      = VK_IMAGE_LAYOUT_UNDEFINED
    , _newLayout      = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
    , _srcAccessMask  = VK_ZERO_FLAGS
    , _dstAccessMask  = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT .|. VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    , _srcStageMask   = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
    , _dstStageMask   = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT }
transitionDependent Undef_ColorAtt = TransitionDependent
    { _oldLayout      = VK_IMAGE_LAYOUT_UNDEFINED
    , _newLayout      = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , _srcAccessMask  = VK_ZERO_FLAGS
    , _dstAccessMask  = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    , _srcStageMask   = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
    , _dstStageMask   = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT }

nextMipmapSize :: Int32 -> Int32
nextMipmapSize n = if 1 < n then (div n 2) else 1

barrierStruct :: VkImage
              -> Word32
              -> VkImageLayout
              -> VkImageLayout
              -> VkAccessFlags
              -> VkAccessFlags
              -> VkImageMemoryBarrier
barrierStruct image mipLevel oldLayout newLayout srcAccessMask dstAccessMask =
    createVk @VkImageMemoryBarrier
        $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
        &* set @"pNext" VK_NULL
        &* set @"oldLayout" oldLayout
        &* set @"newLayout" newLayout
        &* set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
        &* set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
        &* set @"image" image
        &* setVk @"subresourceRange"
            (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
            &* set @"baseMipLevel" mipLevel
            &* set @"levelCount" 1
            &* set @"baseArrayLayer" 0
            &* set @"layerCount" 1 )
        &* set @"srcAccessMask" srcAccessMask
        &* set @"dstAccessMask" dstAccessMask

blitStruct :: VkImage -> Word32 -> Int32 -> Int32 -> VkImageBlit
blitStruct image mipLevel srcWidth srcHeight =
    createVk @VkImageBlit
        $  setAt @"srcOffsets" @0
            (createVk
                $  set @"x" 0
                &* set @"y" 0
                &* set @"z" 0)
        &* setAt @"srcOffsets" @1
            (createVk
                $  set @"x" srcWidth
                &* set @"y" srcHeight
                &* set @"z" 1)
        &* setAt @"dstOffsets" @0
            (createVk
                $  set @"x" 0
                &* set @"y" 0
                &* set @"z" 0)
        &* setAt @"dstOffsets" @1
            (createVk
                $  set @"x" (nextMipmapSize srcWidth)
                &* set @"y" (nextMipmapSize srcHeight)
                &* set @"z" 1)
        &* setVk @"srcSubresource"
            (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
            &* set @"mipLevel" (mipLevel - 1)
            &* set @"baseArrayLayer" 0
            &* set @"layerCount" 1)
        &* setVk @"dstSubresource"
            (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
            &* set @"mipLevel" mipLevel
            &* set @"baseArrayLayer" 0
            &* set @"layerCount" 1)


generateMipmaps :: VkPhysicalDevice
                -> VkImage
                -> VkFormat
                -> Int32
                -> Int32
                -> Word32
                -> VkCommandBuffer
                -> IO ()
generateMipmaps physicalDevice image format width height mipLevels commandBuffer = do
    formatProps <- allocaPeek $ \propsPtr ->
        vkGetPhysicalDeviceFormatProperties physicalDevice format propsPtr
    let supported = getField @"optimalTilingFeatures" formatProps
                    .&. VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
        in when (supported == VK_ZERO_FLAGS) $ throwVKMsg "texture image format does not support linear blitting!"
    mapM_ createMipmap
        (zip3 [1 .. mipLevels-1] (iterate nextMipmapSize (fromIntegral width)) (iterate nextMipmapSize (fromIntegral height)))
    let barrier = barrierStruct
            image
            (mipLevels - 1)
            VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            VK_ACCESS_TRANSFER_WRITE_BIT
            VK_ACCESS_SHADER_READ_BIT
        in withPtr barrier $ \barrierPtr ->
            vkCmdPipelineBarrier commandBuffer
                VK_PIPELINE_STAGE_TRANSFER_BIT
                VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
                VK_ZERO_FLAGS
                0 VK_NULL
                0 VK_NULL
                1 barrierPtr
    where
        createMipmap :: (Word32, Int32, Int32) -> IO ()
        createMipmap (mipLevel, srcWidth, srcHeight) = do
            let barrier = barrierStruct
                    image
                    (mipLevel - 1)
                    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                    VK_ACCESS_TRANSFER_WRITE_BIT
                    VK_ACCESS_TRANSFER_READ_BIT
            withPtr barrier $ \barrierPtr ->
                vkCmdPipelineBarrier commandBuffer
                    VK_PIPELINE_STAGE_TRANSFER_BIT
                    VK_PIPELINE_STAGE_TRANSFER_BIT
                    VK_ZERO_FLAGS
                    0 VK_NULL
                    0 VK_NULL
                    1 barrierPtr

            withPtr (blitStruct image mipLevel srcWidth srcHeight) $ \blitPtr ->
                vkCmdBlitImage commandBuffer
                    image VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                    image VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                    1
                    blitPtr
                    VK_FILTER_LINEAR

            let barrier = barrierStruct
                    image
                    (mipLevel - 1)
                    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                    VK_ACCESS_TRANSFER_READ_BIT
                    VK_ACCESS_SHADER_READ_BIT
            withPtr barrier $ \barrierPtr ->
                vkCmdPipelineBarrier commandBuffer
                    VK_PIPELINE_STAGE_TRANSFER_BIT
                    VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
                    VK_ZERO_FLAGS
                    0 VK_NULL
                    0 VK_NULL
                    1 barrierPtr


findSupportedFormat :: VkPhysicalDevice
                    -> [VkFormat]
                    -> VkImageTiling
                    -> VkFormatFeatureFlags
                    -> IO VkFormat
findSupportedFormat physicalDevice formats tiling features = do
    goodCands <- flip filterM formats $ \format -> do
        props <- allocaPeek $ \propsPtr ->
            vkGetPhysicalDeviceFormatProperties physicalDevice format propsPtr
        return $ case tiling of
            VK_IMAGE_TILING_LINEAR -> getField @"linearTilingFeatures" props .&. features == features
            VK_IMAGE_TILING_OPTIMAL -> getField @"optimalTilingFeatures" props .&. features == features
            otherwise -> False
    case goodCands of
        x:_ -> return x
        []  -> throwVKMsg "failed to find supported format"

findDepthFormat :: VkPhysicalDevice -> IO VkFormat
findDepthFormat physicalDevice =
    findSupportedFormat
        physicalDevice
        [VK_FORMAT_D32_SFLOAT, VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT]
        VK_IMAGE_TILING_OPTIMAL
        VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT


createTextureSampler :: VkDevice -> Word32 -> VkBool32 -> IO VkSampler
createTextureSampler device mipLevels anisotropyEnable = do
    let samplerCreateInfo = createVk @VkSamplerCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
            &* set @"pNext" VK_NULL_HANDLE
            &* set @"magFilter" VK_FILTER_LINEAR
            &* set @"minFilter" VK_FILTER_LINEAR
            &* set @"addressModeU" VK_SAMPLER_ADDRESS_MODE_REPEAT
            &* set @"addressModeV" VK_SAMPLER_ADDRESS_MODE_REPEAT
            &* set @"addressModeW" VK_SAMPLER_ADDRESS_MODE_REPEAT
            &* set @"anisotropyEnable" anisotropyEnable
            &* set @"maxAnisotropy" 16
            &* set @"borderColor" VK_BORDER_COLOR_INT_OPAQUE_BLACK
            &* set @"unnormalizedCoordinates" VK_FALSE
            &* set @"compareEnable" VK_FALSE
            &* set @"compareOp" VK_COMPARE_OP_ALWAYS
            &* set @"mipmapMode" VK_SAMPLER_MIPMAP_MODE_LINEAR
            &* set @"mipLodBias" 0
            &* set @"minLod" 0
            &* set @"maxLod" (fromIntegral mipLevels)
    withPtr samplerCreateInfo $ \samplerCreateInfoPtr ->
      allocaPeek $ \samplerPtr ->
        vkCreateSampler device samplerCreateInfoPtr VK_NULL samplerPtr

destroyTextureSampler :: VkDevice -> VkSampler -> IO ()
destroyTextureSampler device sampler = vkDestroySampler device sampler VK_NULL

createImageView :: VkDevice
                -> VkImage
                -> VkFormat
                -> VkImageAspectFlags
                -> Word32
                -> IO VkImageView
createImageView device image format aspectFlags mipLevels = do
    let componentMapping = createVk @VkComponentMapping
            $  set @"r" VK_COMPONENT_SWIZZLE_IDENTITY
            &* set @"g" VK_COMPONENT_SWIZZLE_IDENTITY
            &* set @"b" VK_COMPONENT_SWIZZLE_IDENTITY
            &* set @"a" VK_COMPONENT_SWIZZLE_IDENTITY
        subresourceRange = createVk @VkImageSubresourceRange
            $  set @"aspectMask" aspectFlags
            &* set @"baseMipLevel" 0
            &* set @"levelCount" mipLevels
            &* set @"baseArrayLayer" 0
            &* set @"layerCount" 1
        imageViewCreateInfo = createVk @VkImageViewCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
            &* set @"pNext" VK_NULL_HANDLE
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"image" image
            &* set @"viewType" VK_IMAGE_VIEW_TYPE_2D
            &* set @"format" format
            &* set @"components" componentMapping
            &* set @"subresourceRange" subresourceRange
    withPtr imageViewCreateInfo $ \imageViewCreateInfoPtr ->
        allocaPeek $ \imageViewPtr ->
            vkCreateImageView device imageViewCreateInfoPtr VK_NULL imageViewPtr

destroyImageView :: VkDevice -> VkImageView -> IO ()
destroyImageView device imageView = vkDestroyImageView device imageView VK_NULL

transitionImageLayout :: VkImage
                      -> VkFormat
                      -> ImageLayoutTransition
                      -> Word32
                      -> VkCommandBuffer
                      -> IO ()
transitionImageLayout image format transition mipLevels commandBuffer = do
    let TransitionDependent {..} = transitionDependent transition
        hasStencilFormats = [VK_FORMAT_D32_SFLOAT_S8_UINT, VK_FORMAT_D24_UNORM_S8_UINT]
        aspectMask = case _newLayout of
            VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                | elem format hasStencilFormats -> VK_IMAGE_ASPECT_DEPTH_BIT .|. VK_IMAGE_ASPECT_STENCIL_BIT
                | otherwise -> VK_IMAGE_ASPECT_DEPTH_BIT
            _ -> VK_IMAGE_ASPECT_COLOR_BIT
        barrier = createVk @VkImageMemoryBarrier
            $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
            &* set @"pNext" VK_NULL
            &* set @"oldLayout" _oldLayout
            &* set @"newLayout" _newLayout
            &* set @"srcQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
            &* set @"dstQueueFamilyIndex" VK_QUEUE_FAMILY_IGNORED
            &* set @"image" image
            &* setVk @"subresourceRange"
                (  set @"aspectMask" aspectMask
                &* set @"baseMipLevel" 0
                &* set @"levelCount" mipLevels
                &* set @"baseArrayLayer" 0
                &* set @"layerCount" 1)
            &* set @"srcAccessMask" _srcAccessMask
            &* set @"dstAccessMask" _dstAccessMask
    withPtr barrier $ \barrierPtr -> vkCmdPipelineBarrier
        commandBuffer
        _srcStageMask
        _dstStageMask
        VK_ZERO_FLAGS
        0 VK_NULL
        0 VK_NULL
        1 barrierPtr


createImage :: VkPhysicalDevice
            -> VkDevice
            -> Word32
            -> Word32
            -> Word32
            -> VkSampleCountFlagBits
            -> VkFormat
            -> VkImageTiling
            -> VkImageUsageFlags
            -> VkMemoryPropertyFlags
            -> IO (VkDeviceMemory, VkImage)
createImage physicalDevice device width height mipLevels samples format tiling usage flags = do
    let imageCreateInfo = createVk @VkImageCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"imageType" VK_IMAGE_TYPE_2D
            &* setVk @"extent"
                (  set @"width" width
                &* set @"height" height
                &* set @"depth" 1)
            &* set @"mipLevels" mipLevels
            &* set @"arrayLayers" 1
            &* set @"format" format
            &* set @"tiling" tiling
            &* set @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
            &* set @"usage" usage
            &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
            &* set @"samples" samples
            &* set @"queueFamilyIndexCount" 0
            &* set @"pQueueFamilyIndices" VK_NULL
    image <- withPtr imageCreateInfo $ \imageCreateInfoPtr -> allocaPeek $ \imagePtr ->
        vkCreateImage device imageCreateInfoPtr VK_NULL imagePtr
    memoryRequirements <- allocaPeek $ \memoryRequirementsPtr ->
        vkGetImageMemoryRequirements device image memoryRequirementsPtr
    memoryType <- findMemoryType physicalDevice(getField @"memoryTypeBits" memoryRequirements) flags
    let allocInfo = createVk @VkMemoryAllocateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"allocationSize" (getField @"size" memoryRequirements)
            &* set @"memoryTypeIndex" memoryType
    imageMemory <- withPtr allocInfo $ \allocInfoPtr ->
        allocaPeek $ \imageMemoryPtr ->
            vkAllocateMemory device allocInfoPtr VK_NULL imageMemoryPtr
    vkBindImageMemory device image imageMemory 0
    return (imageMemory, image)

destroyImage :: VkDevice -> VkImage -> VkDeviceMemory -> IO ()
destroyImage device image imageMemory = do
    vkDestroyImage device image VK_NULL
    vkFreeMemory device imageMemory VK_NULL

copyBufferToImage :: VkDevice
                  -> VkCommandPool
                  -> VkQueue
                  -> VkBuffer
                  -> VkImage
                  -> Word32
                  -> Word32
                  -> IO ()
copyBufferToImage device commandBufferPool commandQueue buffer image width height =
    runCommandsOnce device commandBufferPool commandQueue $ \commandBuffer ->
        let region = createVk @VkBufferImageCopy
                $  set @"bufferOffset" 0
                &* set @"bufferRowLength" 0
                &* set @"bufferImageHeight" 0
                &* setVk @"imageSubresource"
                    (  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
                    &* set @"mipLevel" 0
                    &* set @"baseArrayLayer" 0
                    &* set @"layerCount" 1)
                &* setVk @"imageOffset"
                    (  set @"x" 0
                    &* set @"y" 0
                    &* set @"z" 0)
                &* setVk @"imageExtent"
                    (  set @"width" width
                    &* set @"height" height
                    &* set @"depth" 1)
        in withPtr region $ \regionPtr ->
            vkCmdCopyBufferToImage commandBuffer buffer image VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL 1 regionPtr

createDepthImageView :: VkPhysicalDevice
                     -> VkDevice
                     -> VkCommandPool
                     -> VkQueue
                     -> VkExtent2D
                     -> VkSampleCountFlagBits
                     -> IO TextureData
createDepthImageView physicalDevice device commandBufferPool queue extent samples = do
    let mipLevels = 1
    depthFormat <- findDepthFormat physicalDevice
    (depthImageMemory, depthImage) <- createImage
        physicalDevice
        device
        (getField @"width" extent)
        (getField @"height" extent)
        mipLevels
        samples
        depthFormat
        VK_IMAGE_TILING_OPTIMAL
        VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    depthImageView <- createImageView device depthImage depthFormat VK_IMAGE_ASPECT_DEPTH_BIT mipLevels
    runCommandsOnce device commandBufferPool queue $ \commandBuffer ->
        transitionImageLayout depthImage depthFormat Undef_DepthStencilAtt mipLevels commandBuffer
    let anisotropyEnable = VK_FALSE
    textureSampler <- createTextureSampler device mipLevels anisotropyEnable
    let textureData = TextureData
            { _imageView = depthImageView
            , _image = depthImage
            , _imageMemory = depthImageMemory
            , _textureSampler = textureSampler
            , _imageMipLevels = mipLevels }
    logInfo "createDepthImageView"
    logInfo $ "    Format : " ++ show depthFormat
    logInfo $ "    Size : " ++ show extent
    logInfo $ "    TextureData : " ++ show textureData
    return textureData


createColorImageView :: VkPhysicalDevice
                     -> VkDevice
                     -> VkCommandPool
                     -> VkQueue
                     -> VkFormat
                     -> VkExtent2D
                     -> VkSampleCountFlagBits
                     -> IO TextureData
createColorImageView physicalDevice device commandBufferPool queue format extent samples = do
    let mipLevels = 1
    (colorImageMemory, colorImage) <- createImage
        physicalDevice
        device
        (getField @"width" extent)
        (getField @"height" extent)
        mipLevels
        samples format
        VK_IMAGE_TILING_OPTIMAL
        -- not sure why tutorial uses VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
        (VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT .|. VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    colorImageView <- createImageView device colorImage format VK_IMAGE_ASPECT_COLOR_BIT mipLevels
    runCommandsOnce device commandBufferPool queue $ \commandBuffer ->
        transitionImageLayout colorImage format Undef_ColorAtt mipLevels commandBuffer
    let anisotropyEnable = VK_FALSE
    textureSampler <- createTextureSampler device mipLevels anisotropyEnable
    let textureData = TextureData
            { _imageView = colorImageView
            , _image = colorImage
            , _imageMemory = colorImageMemory
            , _textureSampler = textureSampler
            , _imageMipLevels = mipLevels }
    logInfo "createColorImageView"
    logInfo $ "    Format : " ++ show format
    logInfo $ "    MultiSampleCount : " ++ show samples
    logInfo $ "    Size : " ++ show extent
    logInfo $ "    TextureData : " ++ show textureData
    return textureData


createTextureImageView :: VkPhysicalDevice
                       -> VkDevice
                       -> VkCommandPool
                       -> VkQueue
                       -> VkBool32
                       -> FilePath
                       -> IO TextureData
createTextureImageView physicalDevice device commandBufferPool commandQueue anisotropyEnable filePath = do
    Image { imageWidth, imageHeight, imageData } <- (readImage filePath) >>= \case
        Left err -> throwVKMsg err
        Right dynamicImage -> pure $ convertRGBA8 dynamicImage
    let (imageDataForeignPtr, imageDataLen) = Vec.unsafeToForeignPtr0 imageData
        bufferSize = (fromIntegral imageDataLen)::VkDeviceSize
        mipLevels = (floor . logBase (2::Float) . fromIntegral $ max imageWidth imageHeight) + 1
        format = VK_FORMAT_R8G8B8A8_UNORM
    -- we don't need to access the VkDeviceMemory of the image, copyBufferToImage works with the VkImage
    (imageMemory, image) <- createImage
        physicalDevice
        device
        (fromIntegral imageWidth)
        (fromIntegral imageHeight)
        mipLevels
        VK_SAMPLE_COUNT_1_BIT
        format
        VK_IMAGE_TILING_OPTIMAL
        (VK_IMAGE_USAGE_TRANSFER_SRC_BIT .|. VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT)
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    -- run command
    runCommandsOnce device commandBufferPool commandQueue $ \commandBuffer ->
        transitionImageLayout image format Undef_TransDst mipLevels commandBuffer

    -- create temporary staging buffer
    let stagingBufferUsageFlags = VK_BUFFER_USAGE_TRANSFER_SRC_BIT
        stagingBufferMemoryPropertyFlags = (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
    (stagingBufferMemory, stagingBuffer) <- createBuffer physicalDevice device bufferSize stagingBufferUsageFlags stagingBufferMemoryPropertyFlags

    -- upload data
    stagingDataPtr <- allocaPeek $
        vkMapMemory device stagingBufferMemory 0 bufferSize VK_ZERO_FLAGS
    withForeignPtr imageDataForeignPtr $ \imageDataPtr ->
        copyArray (castPtr stagingDataPtr) imageDataPtr imageDataLen
    vkUnmapMemory device stagingBufferMemory

    -- copy image
    copyBufferToImage device commandBufferPool commandQueue stagingBuffer image
        (fromIntegral imageWidth) (fromIntegral imageHeight)
    runCommandsOnce device commandBufferPool commandQueue $ \commandBuffer ->
        -- generateMipmaps does this as a side effect:
        -- transitionImageLayout image VK_FORMAT_R8G8B8A8_UNORM TransDst_ShaderRO mipLevels
        generateMipmaps
            physicalDevice
            image
            format
            (fromIntegral imageWidth)
            (fromIntegral imageHeight)
            mipLevels
            commandBuffer

    imageView <- createImageView device image format VK_IMAGE_ASPECT_COLOR_BIT mipLevels

    -- destroy temporary staging buffer
    destroyBuffer device stagingBuffer stagingBufferMemory

    textureSampler <- createTextureSampler device mipLevels anisotropyEnable

    let textureData = TextureData
            { _imageView = imageView
            , _image = image
            , _imageMemory = imageMemory
            , _textureSampler = textureSampler
            , _imageMipLevels = mipLevels }

    logInfo "createTextureImageView"
    logInfo $ "    File : " ++ filePath
    logInfo $ "    Format : " ++ show format
    logInfo $ "    Size : " ++ show imageWidth ++ ", " ++ show imageHeight
    logInfo $ "    TextureData : " ++ show textureData

    return textureData

destroyTextureData :: VkDevice -> TextureData -> IO ()
destroyTextureData device textureData@TextureData{..} = do
    logInfo $ "destroyTextureData : " ++ show textureData
    destroyTextureSampler device _textureSampler
    destroyImageView device _imageView
    destroyImage device _image _imageMemory