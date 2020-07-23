module HulkanEngine3D.Render.PostProcess where

data PostProcessData
    = PostProcessData_SSAO
        { _ssao_kernel_size :: {-# UNPACK #-} !Int
        , _ssao_radius :: {-# UNPACK #-} !Float
        , _ssao_noise_dim :: {-# UNPACK #-} !Int
        }
    deriving (Eq, Show)