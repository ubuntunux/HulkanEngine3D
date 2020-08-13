{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HulkanEngine3D.Resource.RenderPassCreateInfo where

import HulkanEngine3D.Render.Renderer
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.CompositeGBuffer as CompositeGBuffer
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderDebug as RenderDebug
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderObject as RenderObject
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderFinal as RenderFinal
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderSSAO as RenderSSAO
import qualified HulkanEngine3D.Resource.RenderPassCreateInfo.RenderShadow as RenderShadow
import HulkanEngine3D.Vulkan.RenderPass


getRenderPassDataCreateInfos :: RendererData -> IO [RenderPassDataCreateInfo]
getRenderPassDataCreateInfos rendererData = do
    render_debug <- RenderDebug.getRenderPassDataCreateInfo rendererData
    render_object <- RenderObject.getRenderPassDataCreateInfo rendererData
    composite_gbuffer <- CompositeGBuffer.getRenderPassDataCreateInfo rendererData
    render_final <- RenderFinal.getRenderPassDataCreateInfo rendererData
    render_ssao <- RenderSSAO.getRenderPassDataCreateInfo rendererData
    render_shadow <- RenderShadow.getRenderPassDataCreateInfo rendererData

    return [ render_debug
           , render_object
           , composite_gbuffer
           , render_final
           , render_ssao
           , render_shadow
           ]