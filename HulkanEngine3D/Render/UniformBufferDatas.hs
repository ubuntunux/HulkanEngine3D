{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveGeneric          #-}

module HulkanEngine3D.Render.UniformBufferDatas where

import GHC.Generics (Generic)

import Graphics.Vulkan
import Numeric.DataFrame

import HulkanEngine3D.Vulkan.UniformBuffer

data UniformBufferDatas = UniformBufferDatas
    { _sceneConstantsBufferData :: UniformBufferData
    } deriving (Eq, Show)

{- |
  Here, in the `view` matrix, we have to decide on our world coordinates.
  This should always be a right-triple, which is forced by lookAt matrix and
  many others. So, here are popular options
    right  up   forward
     x     y      -z     <-- classical OpenGL approach, coincides with projection space
     x     z       y     <-- all positive, z being "up" is kind of intuitive
     ... and a few others, e.g.
     y     z      -x

  I stick x-z-y, because the same triple is used in chalet.obj from the tutorial.
  This affects the choice of axis rotation and lookAt up vector below.

  More about these transformtions can be found be the link:
  https://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
 -}
data SceneConstantsData = SceneConstantsData
  { _VIEW  :: Mat44f
  , _PROJECTION  :: Mat44f
  } deriving (Show, Generic)

instance PrimBytes SceneConstantsData


defaultUniformBufferDatas :: UniformBufferDatas
defaultUniformBufferDatas = UniformBufferDatas
    { _sceneConstantsBufferData = defaultUniformBufferData
    }

createUniformBufferDatas :: VkPhysicalDevice -> VkDevice -> IO UniformBufferDatas
createUniformBufferDatas physicalDevice device = do
    sceneConstantsBufferData <- createUniformBufferData physicalDevice device (bSizeOf @SceneConstantsData undefined)
    return UniformBufferDatas
        { _sceneConstantsBufferData = sceneConstantsBufferData
        }

destroyUniformBufferDatas :: VkDevice -> UniformBufferDatas -> IO ()
destroyUniformBufferDatas device uniformBufferDatas = do
    destroyUniformBufferData device (_sceneConstantsBufferData uniformBufferDatas)