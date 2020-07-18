{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.TextureGenerator where

import Control.Monad.ST
import qualified Data.Vector.Storable as SVector
import qualified Data.Text as Text ()

import Graphics.Vulkan.Core_1_0
import qualified Codec.Picture as Image
import qualified Codec.Picture.Types as Image

import qualified HulkanEngine3D.Render.Renderer as Renderer
import qualified HulkanEngine3D.Vulkan.Texture as Texture
import qualified HulkanEngine3D.Vulkan.Vulkan as Vulkan

generateImage :: Int -> Int -> Image.Image Image.PixelRGBA8
generateImage imageWidth imageHeight = runST $ do
      img <- Image.newMutableImage imageWidth imageHeight
      let go x y
            | x >= imageWidth  = go 0 (y + 1)
            | y >= imageHeight = Image.unsafeFreezeImage img
            | otherwise = do
                Image.writePixel img
                  (imageWidth - x - 1)
                  (imageHeight - y - 1)
                  (Image.PixelRGBA8 255 255 255 255)
                go (x + 1) y
      go 0 0

generateTextures :: Renderer.RendererData -> IO [Texture.TextureData]
generateTextures rendererData = do
    Image.savePngImage "Resource/Textures/a.png" (Image.ImageRGBA8 $ generateImage 4 4)

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