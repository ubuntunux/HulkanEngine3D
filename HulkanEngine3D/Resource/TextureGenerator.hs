{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.TextureGenerator where

import qualified Data.Vector.Storable as SVector
import qualified Data.Text as Text ()

import Graphics.Vulkan.Core_1_0

import qualified HulkanEngine3D.Render.Renderer as Renderer
import qualified HulkanEngine3D.Vulkan.Texture as Texture
import qualified HulkanEngine3D.Vulkan.Vulkan as Vulkan

generateTextures :: Renderer.RendererData -> IO [Texture.TextureData]
generateTextures rendererData = do
    let white = Vulkan.getColor32 255 255 255 255
        black = Vulkan.getColor32 0 0 0 255
        textureCreateInfo = Texture.defaultTextureCreateInfo
            { Texture._textureCreateInfoWidth = 2
            , Texture._textureCreateInfoHeight = 2
            , Texture._textureCreateInfoMinFilter = VK_FILTER_NEAREST
            , Texture._textureCreateInfoMagFilter = VK_FILTER_NEAREST
            , Texture._textureCreateInfoData = SVector.unsafeCast $ SVector.fromList [white, black, black, white]
            }
    textureCheck <- Renderer.createTexture rendererData "check" textureCreateInfo
    return [textureCheck]