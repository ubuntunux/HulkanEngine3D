{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HulkanEngine3D.Vulkan.Texture
    ( RenderTargetCreateInfo (..)
    , TextureData (..)
    , createDescriptorImageInfo
    , findSupportedFormat
    , createImageSampler
    , destroyImageSampler
    , createImageView
    , destroyImageView
    , createImage
    , destroyImage
    , copyBufferToImage
    , createRenderTarget
    , createTextureData
    , destroyTextureData
    ) where

import qualified Data.Text as Text
import Control.Monad
import Codec.Picture
import Data.Bits
import qualified Data.Vector.Storable as Vec
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (castPtr)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

import qualified HulkanEngine3D.Constants as Constants
import HulkanEngine3D.Vulkan.Vulkan
import HulkanEngine3D.Vulkan.Buffer
import HulkanEngine3D.Utilities.Logger
import HulkanEngine3D.Utilities.System


data RenderTargetCreateInfo = RenderTargetCreateInfo
    { _renderTargetWidth' :: Word32
    , _renderTargetHeight' :: Word32
    , _renderTargetDepth' :: Word32
    , _renderTargetFormat' :: VkFormat
    , _renderTargetSamples' :: VkSampleCountFlagBits
    , _renderTargetMinFilter' :: VkFilter
    , _renderTargetMagFilter' :: VkFilter
    , _renderTargetWrapMode' :: VkSamplerAddressMode
    , _renderTargetEnableMipmap' :: Bool
    , _renderTargetEnableAnisotropy' :: Bool
    }

data TextureData = TextureData
    { _textureDataName :: Text.Text
    , _image :: VkImage
    , _imageView :: VkImageView
    , _imageMemory :: VkDeviceMemory
    , _imageSampler ::VkSampler
    , _imageFormat :: VkFormat
    , _imageWidth :: Word32
    , _imageHeight :: Word32
    , _imageDepth :: Word32
    , _imageMipLevels :: Word32
    , _imageSampleCount :: VkSampleCountFlagBits
    , _descriptorImageInfo :: VkDescriptorImageInfo
    } deriving (Eq, Show)

data ImageLayoutTransition = TransferUndef_TransferDst | TransferDst_ShaderReadOnly | TransferUndef_DepthStencilAttachemnt | TransferUndef_ColorAttachemnt

data TransitionDependent = TransitionDependent
    { _oldLayout     :: VkImageLayout
    , _newLayout     :: VkImageLayout
    , _srcAccessMask :: VkAccessFlags
    , _dstAccessMask :: VkAccessFlags
    , _srcStageMask  :: VkPipelineStageFlags
    , _dstStageMask  :: VkPipelineStageFlags }

transitionDependent :: ImageLayoutTransition -> TransitionDependent
transitionDependent TransferUndef_TransferDst = TransitionDependent
    { _oldLayout      = VK_IMAGE_LAYOUT_UNDEFINED
    , _newLayout      = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    , _srcAccessMask  = VK_ZERO_FLAGS
    , _dstAccessMask  = VK_ACCESS_TRANSFER_WRITE_BIT
    , _srcStageMask   = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
    , _dstStageMask   = VK_PIPELINE_STAGE_TRANSFER_BIT }
transitionDependent TransferDst_ShaderReadOnly = TransitionDependent
    { _oldLayout      = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    , _newLayout      = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    , _srcAccessMask  = VK_ACCESS_TRANSFER_WRITE_BIT
    , _dstAccessMask  = VK_ACCESS_SHADER_READ_BIT
    , _srcStageMask   = VK_PIPELINE_STAGE_TRANSFER_BIT
    , _dstStageMask   = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT }
transitionDependent TransferUndef_DepthStencilAttachemnt = TransitionDependent
    { _oldLayout      = VK_IMAGE_LAYOUT_UNDEFINED
    , _newLayout      = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
    , _srcAccessMask  = VK_ZERO_FLAGS
    , _dstAccessMask  = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT .|. VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    , _srcStageMask   = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
    , _dstStageMask   = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT }
transitionDependent TransferUndef_ColorAttachemnt = TransitionDependent
    { _oldLayout      = VK_IMAGE_LAYOUT_UNDEFINED
    , _newLayout      = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , _srcAccessMask  = VK_ZERO_FLAGS
    , _dstAccessMask  = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    , _srcStageMask   = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
    , _dstStageMask   = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT }

nextMipmapSize :: Int32 -> Int32
nextMipmapSize n = if 1 < n then (div n 2) else 1

createDescriptorImageInfo :: VkImageLayout -> VkImageView -> VkSampler -> VkDescriptorImageInfo
createDescriptorImageInfo imageLayout imageView imageSasmpler =
    createVk @VkDescriptorImageInfo
        $  set @"imageLayout" imageLayout -- VK_IMAGE_LAYOUT_GENERAL
        &* set @"imageView" imageView
        &* set @"sampler" imageSasmpler

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
    let supported = (getField @"optimalTilingFeatures" formatProps) .&. VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
    when (supported == VK_ZERO_FLAGS) $ throwVKMsg "texture image format does not support linear blitting!"
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
                    -> VkFormat
                    -> VkImageTiling
                    -> VkFormatFeatureFlags
                    -> IO VkFormat
findSupportedFormat physicalDevice requireFormat tiling features = do
    goodCands <- flip filterM Constants.depthFomats $ \format -> do
        props <- allocaPeek $ \propsPtr ->
            vkGetPhysicalDeviceFormatProperties physicalDevice format propsPtr
        return $ case tiling of
            VK_IMAGE_TILING_LINEAR -> getField @"linearTilingFeatures" props .&. features == features
            VK_IMAGE_TILING_OPTIMAL -> getField @"optimalTilingFeatures" props .&. features == features
            otherwise -> False
    case goodCands of
        x:_ -> return $ if elem requireFormat goodCands then requireFormat else x
        []  -> throwVKMsg "failed to find supported format"


createImageSampler :: VkDevice -> Word32 -> VkFilter -> VkFilter -> VkSamplerAddressMode -> VkBool32 -> IO VkSampler
createImageSampler device mipLevels minFilter magFilter samplerAddressMode anisotropyEnable = do
    let samplerCreateInfo = createVk @VkSamplerCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
            &* set @"pNext" VK_NULL_HANDLE
            &* set @"magFilter" minFilter
            &* set @"minFilter" magFilter
            &* set @"addressModeU" samplerAddressMode
            &* set @"addressModeV" samplerAddressMode
            &* set @"addressModeW" samplerAddressMode
            &* set @"anisotropyEnable" anisotropyEnable
            &* set @"maxAnisotropy" 16
            &* set @"borderColor" VK_BORDER_COLOR_INT_OPAQUE_BLACK
            &* set @"unnormalizedCoordinates" VK_FALSE
            &* set @"compareEnable" VK_FALSE
            &* set @"compareOp" VK_COMPARE_OP_NEVER
            &* set @"mipmapMode" VK_SAMPLER_MIPMAP_MODE_LINEAR
            &* set @"mipLodBias" 0
            &* set @"minLod" 0
            &* set @"maxLod" (fromIntegral mipLevels)
    withPtr samplerCreateInfo $ \samplerCreateInfoPtr ->
      allocaPeek $ \samplerPtr ->
        vkCreateSampler device samplerCreateInfoPtr VK_NULL samplerPtr

destroyImageSampler :: VkDevice -> VkSampler -> IO ()
destroyImageSampler device sampler = vkDestroySampler device sampler VK_NULL

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
        aspectMask = case _newLayout of
            VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                | elem format Constants.depthStencilFormats -> VK_IMAGE_ASPECT_DEPTH_BIT .|. VK_IMAGE_ASPECT_STENCIL_BIT
                | otherwise -> VK_IMAGE_ASPECT_DEPTH_BIT
            otherwise -> VK_IMAGE_ASPECT_COLOR_BIT
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
            -> Word32
            -> VkSampleCountFlagBits
            -> VkFormat
            -> VkImageTiling
            -> VkImageUsageFlags
            -> VkMemoryPropertyFlags
            -> IO (VkDeviceMemory, VkImage)
createImage physicalDevice device width height depth mipLevels samples format tiling usage flags = do
    let getImageType width height depth
            | (height <= 1), (depth <= 1) = VK_IMAGE_TYPE_1D
            | (depth <= 1) = VK_IMAGE_TYPE_2D
            | otherwise = VK_IMAGE_TYPE_3D
        imageCreateInfo = createVk @VkImageCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_ZERO_FLAGS
            &* set @"imageType" (getImageType width height depth)
            &* setVk @"extent"
                (  set @"width" width
                &* set @"height" height
                &* set @"depth" depth
                )
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

createRenderTarget :: Text.Text
                   -> VkPhysicalDevice
                   -> VkDevice
                   -> VkCommandPool
                   -> VkQueue
                   -> RenderTargetCreateInfo
                   -> IO TextureData
createRenderTarget textureDataName physicalDevice device commandBufferPool queue renderTargetCreateInfo@RenderTargetCreateInfo {..} = do
    let enableAnisotropy = if _renderTargetEnableAnisotropy' then VK_TRUE else VK_FALSE
        maxImageSize = max _renderTargetWidth' (max _renderTargetHeight' _renderTargetDepth')
        mipLevels = case _renderTargetEnableMipmap' of
            True -> (floor $ logBase (2::Float) (fromIntegral maxImageSize)) + 1
            False -> 1
        isDepthFormat = elem _renderTargetFormat' Constants.depthFomats
    (imageUsage, imageAspect, imageLayoutTransition, imageFormat) <- if isDepthFormat then do
        depthFormat <- findSupportedFormat physicalDevice _renderTargetFormat' VK_IMAGE_TILING_OPTIMAL VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
        return
            ( VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
            , VK_IMAGE_ASPECT_DEPTH_BIT
            , TransferUndef_DepthStencilAttachemnt
            , depthFormat
            )
    else
        return
            -- not sure why tutorial uses VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
            ( VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT .|. VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            , VK_IMAGE_ASPECT_COLOR_BIT
            , TransferUndef_ColorAttachemnt
            , _renderTargetFormat'
            )
    (imageMemory, image) <- createImage
        physicalDevice
        device
        _renderTargetWidth'
        _renderTargetHeight'
        _renderTargetDepth'
        mipLevels
        _renderTargetSamples'
        imageFormat
        VK_IMAGE_TILING_OPTIMAL
        imageUsage
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    runCommandsOnce device commandBufferPool queue $ \commandBuffer ->
        transitionImageLayout image imageFormat imageLayoutTransition mipLevels commandBuffer

    imageView <- createImageView device image imageFormat imageAspect mipLevels
    imageSampler <- createImageSampler device mipLevels _renderTargetMinFilter' _renderTargetMagFilter' _renderTargetWrapMode' enableAnisotropy
    let descriptorImageInfo = createDescriptorImageInfo VK_IMAGE_LAYOUT_GENERAL imageView imageSampler
        textureData = TextureData
            { _textureDataName = textureDataName
            , _imageView = imageView
            , _image = image
            , _imageMemory = imageMemory
            , _imageSampler = imageSampler
            , _imageFormat = imageFormat
            , _imageWidth = _renderTargetWidth'
            , _imageHeight = _renderTargetHeight'
            , _imageDepth = _renderTargetDepth'
            , _imageSampleCount = _renderTargetSamples'
            , _imageMipLevels = mipLevels
            , _descriptorImageInfo = descriptorImageInfo
            }
    logInfo "createRenderTarget"
    logInfo $ "    Name : " ++ Text.unpack textureDataName
    logInfo $ "    Format : " ++ show imageFormat
    logInfo $ "    MultiSampleCount : " ++ show _renderTargetSamples'
    logInfo $ "    Size : " ++ show (_renderTargetWidth', _renderTargetHeight', _renderTargetDepth')
    logInfo $ "    TextureData : image " ++ show image ++ ", imageView " ++ show imageView ++ ", imageMemory " ++ show imageMemory ++ ", sampler " ++ show imageSampler
    return textureData


createTextureData :: Text.Text
                  -> VkPhysicalDevice
                  -> VkDevice
                  -> VkCommandPool
                  -> VkQueue
                  -> VkBool32
                  -> FilePath
                  -> IO TextureData
createTextureData textureDataName physicalDevice device commandBufferPool commandQueue anisotropyEnable filePath = do
    Image { imageWidth, imageHeight, imageData } <- (readImage filePath) >>= \case
        Left err -> throwVKMsg err
        Right dynamicImage -> pure $ convertRGBA8 dynamicImage
    let (imageDataForeignPtr, imageDataLen) = Vec.unsafeToForeignPtr0 imageData
        bufferSize = (fromIntegral imageDataLen)::VkDeviceSize
        mipLevels = (floor . logBase (2::Float) . fromIntegral $ max imageWidth imageHeight) + 1
        format = VK_FORMAT_R8G8B8A8_UNORM
        samples = VK_SAMPLE_COUNT_1_BIT
    -- we don't need to access the VkDeviceMemory of the image, copyBufferToImage works with the VkImage
    (imageMemory, image) <- createImage
        physicalDevice
        device
        (fromIntegral imageWidth)
        (fromIntegral imageHeight)
        1
        mipLevels
        samples
        format
        VK_IMAGE_TILING_OPTIMAL
        (VK_IMAGE_USAGE_TRANSFER_SRC_BIT .|. VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT)
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    -- run command
    runCommandsOnce device commandBufferPool commandQueue $ \commandBuffer ->
        transitionImageLayout image format TransferUndef_TransferDst mipLevels commandBuffer

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

    copyBufferToImage device commandBufferPool commandQueue stagingBuffer image
        (fromIntegral imageWidth) (fromIntegral imageHeight)
    runCommandsOnce device commandBufferPool commandQueue $ \commandBuffer ->
        -- generateMipmaps does this as a side effect:
        -- transitionImageLayout image VK_FORMAT_R8G8B8A8_UNORM TransferDst_ShaderReadOnly mipLevels
        generateMipmaps
            physicalDevice
            image
            format
            (fromIntegral imageWidth)
            (fromIntegral imageHeight)
            mipLevels
            commandBuffer
    destroyBuffer device stagingBuffer stagingBufferMemory

    imageView <- createImageView device image format VK_IMAGE_ASPECT_COLOR_BIT mipLevels
    imageSampler <- createImageSampler device mipLevels VK_FILTER_LINEAR VK_FILTER_LINEAR VK_SAMPLER_ADDRESS_MODE_REPEAT anisotropyEnable
    let descriptorImageInfo = createDescriptorImageInfo VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL imageView imageSampler
        imageDepth = 1::Word32
        textureData@TextureData {..} = TextureData
            { _textureDataName = textureDataName
            , _image = image
            , _imageView = imageView
            , _imageMemory = imageMemory
            , _imageSampler = imageSampler
            , _imageFormat = format
            , _imageWidth = fromIntegral imageWidth
            , _imageHeight = fromIntegral imageHeight
            , _imageDepth = fromIntegral imageDepth
            , _imageSampleCount = samples
            , _imageMipLevels = fromIntegral mipLevels
            , _descriptorImageInfo = descriptorImageInfo
            }

    logInfo "createTextureData"
    logInfo $ "    Name : " ++ Text.unpack textureDataName
    logInfo $ "    File : " ++ filePath
    logInfo $ "    Format : " ++ show format
    logInfo $ "    Size : " ++ show imageWidth ++ ", " ++ show imageHeight ++ ", " ++ show imageDepth
    logInfo $ "    TextureData : image " ++ show _image ++ ", imageView " ++ show _imageView ++ ", imageMemory " ++ show _imageMemory ++ ", sampler " ++ show _imageSampler

    return textureData

destroyTextureData :: VkDevice -> TextureData -> IO ()
destroyTextureData device textureData@TextureData{..} = do
    logInfo $ "destroyTextureData(" ++ (Text.unpack _textureDataName) ++ ") : image " ++ show _image ++ ", imageView " ++ show _imageView ++ ", imageMemory " ++ show _imageMemory ++ ", sampler " ++ show _imageSampler
    destroyImageSampler device _imageSampler
    destroyImageView device _imageView
    destroyImage device _image _imageMemory