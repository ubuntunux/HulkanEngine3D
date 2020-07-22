{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}

module HulkanEngine3D.Resource.TextureGenerator where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Storable as SVector
import qualified Data.Text as Text ()
import System.Directory
import System.FilePath.Posix

import Graphics.Vulkan.Core_1_0
import qualified Codec.Picture as Image
import qualified Codec.Picture.Types as Image

import qualified HulkanEngine3D.Render.Renderer as Renderer
import qualified HulkanEngine3D.Vulkan.Texture as Texture
import qualified HulkanEngine3D.Vulkan.Vulkan as Vulkan

generateTextureExample :: Renderer.RendererData -> IO Texture.TextureData
generateTextureExample rendererData = do
    let white = Vulkan.getColor32 255 255 255 255
        black = Vulkan.getColor32 0 0 0 255
        textureCreateInfo = Texture.defaultTextureCreateInfo
            { Texture._textureCreateInfoWidth = 2
            , Texture._textureCreateInfoHeight = 2
            , Texture._textureCreateInfoMinFilter = VK_FILTER_NEAREST
            , Texture._textureCreateInfoMagFilter = VK_FILTER_NEAREST
            , Texture._textureCreateInfoData = SVector.unsafeCast $ SVector.fromList [white, black, black, white]
            }
    Renderer.createTexture rendererData "check" textureCreateInfo

generateFlatColorImageRGBA8 :: Int -> Int -> (Word8, Word8, Word8, Word8) -> Image.Image Image.PixelRGBA8
generateFlatColorImageRGBA8 imageWidth imageHeight (r, g, b, a) = runST $ do
      img <- Image.newMutableImage imageWidth imageHeight
      let go x y
            | x >= imageWidth  = go 0 (y + 1)
            | y >= imageHeight = Image.unsafeFreezeImage img
            | otherwise = do
                Image.writePixel img
                  (imageWidth - x - 1)
                  (imageHeight - y - 1)
                  (Image.PixelRGBA8 r g b a)
                go (x + 1) y
      go 0 0

generateFlatColorImageRGBF :: Int -> Int -> (Float, Float, Float) -> Image.Image Image.PixelRGBF
generateFlatColorImageRGBF imageWidth imageHeight (r, g, b) = runST $ do
      img <- Image.newMutableImage imageWidth imageHeight
      let go x y
            | x >= imageWidth  = go 0 (y + 1)
            | y >= imageHeight = Image.unsafeFreezeImage img
            | otherwise = do
                Image.writePixel img
                  (imageWidth - x - 1)
                  (imageHeight - y - 1)
                  (Image.PixelRGBF r g b)
                go (x + 1) y
      go 0 0

generateFlatColorImageRGBA16 :: Int -> Int -> (Word16, Word16, Word16, Word16) -> Image.Image Image.PixelRGBA16
generateFlatColorImageRGBA16 imageWidth imageHeight (r, g, b, a) = runST $ do
      img <- Image.newMutableImage imageWidth imageHeight
      let go x y
            | x >= imageWidth  = go 0 (y + 1)
            | y >= imageHeight = Image.unsafeFreezeImage img
            | otherwise = do
                Image.writePixel img
                  (imageWidth - x - 1)
                  (imageHeight - y - 1)
                  (Image.PixelRGBA16 r g b a)
                go (x + 1) y
      go 0 0

generateTextures :: Renderer.RendererData -> FilePath -> IO ()
generateTextures rendererData textureFilePath = do
    ifNotExistSaveImage textureFilePath "common/flat_none.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (0, 0, 0, 0))
    ifNotExistSaveImage textureFilePath "common/flat_black.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (0, 0, 0, 255))
    ifNotExistSaveImage textureFilePath "common/flat_gray.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (128, 128, 128, 255))
    ifNotExistSaveImage textureFilePath "common/flat_white.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (255, 255, 255, 255))
    ifNotExistSaveImage textureFilePath "common/flat_red.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (255, 0, 0, 255))
    ifNotExistSaveImage textureFilePath "common/flat_green.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (0, 255, 0, 255))
    ifNotExistSaveImage textureFilePath "common/flat_blue.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (0, 0, 255, 255))
    ifNotExistSaveImage textureFilePath "common/flat_normal.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (128, 128, 255, 255))
    ifNotExistSaveImage textureFilePath "common/flat_normal_f32.png" (Image.ImageRGBF $ generateFlatColorImageRGBF 2 2 (0.5, 0.5, 1.0))
    ifNotExistSaveImage textureFilePath "common/flat_normal_u16.png" (Image.ImageRGBA16 $ generateFlatColorImageRGBA16 2 2 (32767, 32767, 65535, 65535))
    ifNotExistSaveImage textureFilePath "common/check.png" (Image.ImageRGBA8 $ generateFlatColorImageRGBA8 2 2 (128, 128, 255, 255))
    where
        ifNotExistSaveImage :: FilePath -> FilePath -> Image.DynamicImage -> IO ()
        ifNotExistSaveImage textureFilePath imageFileName image = do
            let imageFilePath = combine textureFilePath imageFileName
            doesFileExist imageFilePath >>= \result -> when (not result) $ do
                createDirectoryIfMissing True (takeDirectory imageFilePath)
                Image.savePngImage imageFilePath image
        