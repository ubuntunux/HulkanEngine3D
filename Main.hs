module Main
  ( main
  ) where

import Control.Concurrent (forkIO)

import HulkanEngine3D.Application
import HulkanEngine3D.Application.Editor

main::IO()
main = do
    forkIO $ runEditor
    runApplication