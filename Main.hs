module Main
  ( main
  ) where

import Data.IORef
import HulkanEngine3D.Application.Application
import HulkanEngine3D.Application.Command

main::IO()
main = do
    commandToEditor <- newIORef Command_None
    commandToApp <- newIORef Command_None
    runApplication commandToEditor commandToApp
