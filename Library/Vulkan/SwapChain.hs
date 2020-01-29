{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Vulkan.SwapChain
  ( SwapChainSupportDetails (..)
  , SwapChainData (..)
  , createSwapChainData
  , destroySwapChainData
  ) where

import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain

import qualified Library.Constants as Constants
import Library.Utils
import Library.Logger
import Library.Vulkan.Queue


data SwapChainSupportDetails = SwapChainSupportDetails
    { _capabilities :: VkSurfaceCapabilitiesKHR
    , _formats      :: [VkSurfaceFormatKHR]
    , _presentModes :: [VkPresentModeKHR]
    } deriving (Eq, Show)

data SwapChainData = SwapChainData
    { _swapChain :: VkSwapchainKHR
    , _swapChainImageCount :: Word32
    , _swapChainImageFormat :: VkFormat
    , _swapChainImages :: [VkImage]
    , _swapChainImageViews :: [VkImageView]
    , _swapChainExtent :: VkExtent2D
    } deriving (Eq, Show)


chooseSwapSurfaceFormat :: SwapChainSupportDetails -> IO VkSurfaceFormatKHR
chooseSwapSurfaceFormat swapChainSupportDetails =
  if 1 == length formats && VK_FORMAT_UNDEFINED == getFormat (head formats) then
    newVkData $ \surfaceFormatPtr -> do
      writeField @"format" surfaceFormatPtr VK_FORMAT_B8G8R8A8_UNORM
      writeField @"colorSpace" surfaceFormatPtr VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  else
    findAvailableFormat formats
  where
    formats = _formats swapChainSupportDetails
    getFormat = getField @"format"
    getColorSpace = getField @"colorSpace"
    findAvailableFormat :: [VkSurfaceFormatKHR] -> IO VkSurfaceFormatKHR
    findAvailableFormat [] = return $ head formats
    findAvailableFormat (x:xs) =
      if VK_FORMAT_B8G8R8A8_UNORM == getFormat x && VK_COLOR_SPACE_SRGB_NONLINEAR_KHR == getColorSpace x
      then return x
      else findAvailableFormat xs

chooseSwapPresentMode :: SwapChainSupportDetails -> IO VkPresentModeKHR
chooseSwapPresentMode swapChainSupportDetails
  | VK_PRESENT_MODE_FIFO_KHR `elem` presentModes = return VK_PRESENT_MODE_FIFO_KHR
  | VK_PRESENT_MODE_MAILBOX_KHR `elem` presentModes = return VK_PRESENT_MODE_MAILBOX_KHR
  | VK_PRESENT_MODE_FIFO_RELAXED_KHR `elem` presentModes = return VK_PRESENT_MODE_FIFO_RELAXED_KHR
  | VK_PRESENT_MODE_IMMEDIATE_KHR `elem` presentModes = return VK_PRESENT_MODE_IMMEDIATE_KHR
  | otherwise = return VK_PRESENT_MODE_FIFO_KHR
  where
    presentModes = _presentModes swapChainSupportDetails

chooseSwapExtent :: SwapChainSupportDetails -> IO VkExtent2D
chooseSwapExtent swapChainSupportDetails = do
  imageExtent <- newVkData @VkExtent2D $ \extentPtr -> do
    writeField @"width" extentPtr $ max (width $ getField @"minImageExtent" capabilities)
                             $ min (width $ getField @"maxImageExtent" capabilities)
                                   (width $ getField @"currentExtent"  capabilities)
    writeField @"height" extentPtr $ max (height $ getField @"minImageExtent" capabilities)
                              $ min (height $ getField @"maxImageExtent" capabilities)
                                    (height $ getField @"currentExtent"  capabilities)
  return imageExtent
  where
    capabilities = _capabilities swapChainSupportDetails
    width = getField @"width"
    height = getField @"height"

createSwapChainData :: VkDevice
                    -> SwapChainSupportDetails
                    -> QueueFamilyDatas
                    -> VkSurfaceKHR
                    -> IO SwapChainData
createSwapChainData device swapChainSupportDetails queueFamilyDatas vkSurface = do
  surfaceFormat <- chooseSwapSurfaceFormat swapChainSupportDetails
  presentMode <- chooseSwapPresentMode swapChainSupportDetails
  imageExtent <- chooseSwapExtent swapChainSupportDetails
  queueFamilyIndicesPtr <- newArray (_queueFamilyIndexList queueFamilyDatas)

  -- try tripple buffering
  let maxImageCount = getField @"maxImageCount" $ _capabilities swapChainSupportDetails
      minImageCount = getField @"minImageCount" $ _capabilities swapChainSupportDetails
      imageCount' = if maxImageCount <= 0
                   then max minImageCount Constants.imageCount
                   else min maxImageCount $ max minImageCount Constants.imageCount

  -- write VkSwapchainCreateInfoKHR
  swapChainCreateInfo <- newVkData @VkSwapchainCreateInfoKHR $ \swapChainCreateInfoPtr -> do
    writeField @"sType" swapChainCreateInfoPtr VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
    writeField @"pNext" swapChainCreateInfoPtr VK_NULL_HANDLE
    writeField @"flags" swapChainCreateInfoPtr VK_ZERO_FLAGS
    writeField @"surface" swapChainCreateInfoPtr vkSurface
    writeField @"minImageCount" swapChainCreateInfoPtr imageCount'
    writeField @"imageFormat" swapChainCreateInfoPtr (getField @"format" surfaceFormat)
    writeField @"imageColorSpace" swapChainCreateInfoPtr (getField @"colorSpace" surfaceFormat)
    writeField @"imageExtent" swapChainCreateInfoPtr imageExtent
    writeField @"imageArrayLayers" swapChainCreateInfoPtr 1
    writeField @"imageUsage" swapChainCreateInfoPtr VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
    if (_graphicsQueue queueFamilyDatas) /= (_presentQueue queueFamilyDatas)
    then do
      writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_CONCURRENT
      writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr (_queueFamilyCount queueFamilyDatas)
      writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr queueFamilyIndicesPtr
    else do
      writeField @"imageSharingMode" swapChainCreateInfoPtr VK_SHARING_MODE_EXCLUSIVE
      writeField @"queueFamilyIndexCount" swapChainCreateInfoPtr 0
      writeField @"pQueueFamilyIndices" swapChainCreateInfoPtr VK_NULL_HANDLE
    writeField @"preTransform" swapChainCreateInfoPtr (getField @"currentTransform" $ _capabilities swapChainSupportDetails)
    writeField @"compositeAlpha" swapChainCreateInfoPtr VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
    writeField @"presentMode" swapChainCreateInfoPtr presentMode
    writeField @"clipped" swapChainCreateInfoPtr VK_TRUE
    writeField @"oldSwapchain" swapChainCreateInfoPtr VK_NULL_HANDLE

  swapChain <- alloca $ \swapChainPtr -> do
      result <- vkCreateSwapchainKHR device (unsafePtr swapChainCreateInfo) VK_NULL_HANDLE swapChainPtr
      validationVK result "vkCreateSwapchainKHR failed!"
      peek swapChainPtr

  swapChainImages <- asListVK $ \counterPtr valueArrayPtr -> do
      result <- vkGetSwapchainImagesKHR device swapChain counterPtr valueArrayPtr
      validationVK result "vkGetSwapchainImagesKHR error"

  let swapChainImageFormat = getField @"imageFormat" swapChainCreateInfo
      swapChainExtent = (getField @"imageExtent" swapChainCreateInfo)

  touchVkData swapChainCreateInfo
  free queueFamilyIndicesPtr

  swapChainImageViews <- createSwapChainImageViews device swapChainImages swapChainImageFormat

  logInfo $ "Create SwapChain : " ++ (show swapChain)
  logInfo $ "    imageCount : " ++ (show imageCount') ++ " " ++ (show swapChainImages)
  logInfo $ "    imageFormat : " ++ (show $ getField @"imageFormat" swapChainCreateInfo)
  logInfo $ "    imageColorSpace : " ++ (show $ getField @"imageColorSpace" swapChainCreateInfo)
  logInfo $ "    imageViews : " ++ (show swapChainImageViews)
  logInfo $ "    imageExtent : " ++ (show $ getField @"imageExtent" swapChainCreateInfo)
  logInfo $ "    imageSharingMode : " ++ (show $ getField @"imageSharingMode" swapChainCreateInfo)

  let swapChainData = SwapChainData { _swapChain = swapChain
                                    , _swapChainImageCount = fromIntegral (length swapChainImages)
                                    , _swapChainImages = swapChainImages
                                    , _swapChainImageFormat = swapChainImageFormat
                                    , _swapChainImageViews = swapChainImageViews
                                    , _swapChainExtent = swapChainExtent }
  return swapChainData

destroySwapChainData :: VkDevice -> SwapChainData -> IO ()
destroySwapChainData device swapChainData = do
  destroySwapChainImageViews device (_swapChainImageViews swapChainData)
  logInfo "Destroy SwapChain"
  vkDestroySwapchainKHR device (_swapChain swapChainData) VK_NULL_HANDLE

createSwapChainImageViews :: VkDevice -> [VkImage] -> VkFormat -> IO [VkImageView]
createSwapChainImageViews device swapChainImages swapChainImageFormat = do
  components <- (newVkData $ \componentsPtr -> do
    writeField @"r" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
    writeField @"g" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
    writeField @"b" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY
    writeField @"a" componentsPtr VK_COMPONENT_SWIZZLE_IDENTITY)::IO VkComponentMapping
  subresourceRange <- (newVkData $ \subresourceRangePtr -> do
    writeField @"aspectMask" subresourceRangePtr VK_IMAGE_ASPECT_COLOR_BIT
    writeField @"baseMipLevel" subresourceRangePtr 0
    writeField @"levelCount" subresourceRangePtr 1
    writeField @"baseArrayLayer" subresourceRangePtr 0
    writeField @"layerCount" subresourceRangePtr 1)::IO VkImageSubresourceRange
  let
    getImageViewCreateInfo :: VkImage -> IO VkImageViewCreateInfo
    getImageViewCreateInfo image = newVkData @VkImageViewCreateInfo $ \viewPtr -> do
      writeField @"sType" viewPtr VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
      writeField @"pNext" viewPtr VK_NULL_HANDLE
      writeField @"flags" viewPtr VK_ZERO_FLAGS
      writeField @"image" viewPtr image
      writeField @"viewType" viewPtr VK_IMAGE_VIEW_TYPE_2D
      writeField @"format" viewPtr swapChainImageFormat
      writeField @"components" viewPtr components
      writeField @"subresourceRange" viewPtr subresourceRange
  imageViewCreateInfos <- mapM getImageViewCreateInfo swapChainImages
  imageViews <- forM imageViewCreateInfos $ \imageViewCreateInfo ->
    alloca $ \imageViewPtr -> do
        result <- vkCreateImageView device (unsafePtr imageViewCreateInfo) VK_NULL_HANDLE imageViewPtr
        validationVK result "vkCreateImageView error"
        peek imageViewPtr
  mapM_ touchVkData imageViewCreateInfos
  return imageViews

destroySwapChainImageViews :: VkDevice -> [VkImageView] -> IO ()
destroySwapChainImageViews device imageViews = do
  mapM_ (\imageView -> vkDestroyImageView device imageView VK_NULL_HANDLE) imageViews