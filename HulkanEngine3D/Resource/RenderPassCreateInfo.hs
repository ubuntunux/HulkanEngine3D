{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.RenderPassCreateInfo where

import HulkanEngine3D.Render.Renderer
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.CompositeGBuffer as CompositeGBuffer
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderDebug as RenderDebug
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderDefault as RenderDefault
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderFinal as RenderFinal
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.SSAO as SSAO
import HulkanEngine3D.Vulkan.RenderPass

getRenderPassDataCreateInfos :: RendererData -> IO [RenderPassDataCreateInfo]
getRenderPassDataCreateInfos rendererData = do
    render_debug <- RenderDebug.getRenderPassDataCreateInfo rendererData
    render_default <- RenderDefault.getRenderPassDataCreateInfo rendererData
    composite_gbuffer <- CompositeGBuffer.getRenderPassDataCreateInfo rendererData
    render_ssao <- SSAO.getRenderPassDataCreateInfo rendererData
    render_final <- RenderFinal.getRenderPassDataCreateInfo rendererData
    return [ render_debug
           , render_default
           , composite_gbuffer
           , render_ssao
           , render_final
           ]