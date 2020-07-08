module Main
  ( main
  ) where

import Data.IORef
import Control.Concurrent (forkIO)

import Web.Browser

import HulkanEngine3D.Application.Application
import HulkanEngine3D.Application.Command
import HulkanEngine3D.Application.Editor

main::IO()
main = do
    openBrowser "http://127.0.0.1:8023/"
    commandToEditor <- newIORef Command_None
    commandToApp <- newIORef Command_None
    forkIO $ runEditor commandToEditor commandToApp
    runApplication commandToEditor commandToApp
