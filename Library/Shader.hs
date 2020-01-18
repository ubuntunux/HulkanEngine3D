{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}

module Library.Shader
  ( createShaderStageCreateInfo
  , destroyShaderStageCreateInfo  
  , compileGLSL
  )  where

import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Library.Utils


compileGLSL :: FilePath -> IO (Int, Ptr Word32)
compileGLSL filePath = do
  validatorExe <- fromMaybe
        ( error $ unlines
          [ "Cannot find glslangValidator executable."
          , "Check if it is available in your $PATH."
          , "Read more about it at https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"]
        ) <$> findExecutable "glslangValidator"
  tmpDir <- getTemporaryDirectory
  curDir <- getCurrentDirectory
  createDirectoryIfMissing True tmpDir
  let spirvCodeFile = tmpDir </> "haskell-spirv2.tmp"
      shaderFile = curDir </> filePath
      shaderDir = takeDirectory shaderFile
      shaderFName = takeFileName shaderFile
  doesFileExist shaderFile >>= flip unless (error $ "compileGLSL: " ++ shaderFile ++ " does not exist.")
  doesFileExist spirvCodeFile >>= flip when (removeFile spirvCodeFile)

  (exitCode, stdo, stde) <- readCreateProcessWithExitCode
      ((shell $ validatorExe ++ " -V -o " ++ spirvCodeFile ++ " " ++ shaderFName) { cwd = Just shaderDir }) ""

  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure i -> do
      putStrLn stdo
      putStrLn stde
      error $ "glslangValidator exited with code " ++ show i ++ "."

  withBinaryFile spirvCodeFile ReadMode $ \h -> do
    fsize <- hFileSize h
    let contentSize = fromIntegral $ case rem fsize 4 of
          0 -> fsize
          k -> fsize + 4 - k
    contentsPtr <- mallocArray contentSize
    hasRead <- hGetBuf h contentsPtr contentSize
    unless (contentSize /= hasRead) $ do
      contents <- peekArray hasRead contentsPtr
      pokeArray contentsPtr (contents ++ (replicate (contentSize - hasRead) 0))
    return (contentSize, contentsPtr)

getShaderModuleCreateInfo :: Int -> Ptr Word32 -> IO VkShaderModuleCreateInfo
getShaderModuleCreateInfo codeSize codePtr = return $ createVk @VkShaderModuleCreateInfo
  $  set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  &* set @"pNext" VK_NULL
  &* set @"codeSize" (fromIntegral codeSize)
  &* set @"pCode" codePtr
  &* set @"flags" VK_ZERO_FLAGS

getShaderCreateInfo :: VkShaderStageFlagBits -> VkShaderModule -> VkPipelineShaderStageCreateInfo
getShaderCreateInfo stageBit shaderModule = createVk @VkPipelineShaderStageCreateInfo
  $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  &* set @"pNext" VK_NULL
  &* set @"stage" stageBit
  &* set @"module" shaderModule
  &* setStrRef @"pName" "main"

createShaderStageCreateInfo :: VkDevice -> String -> VkShaderStageFlagBits -> IO VkPipelineShaderStageCreateInfo
createShaderStageCreateInfo device shaderFilePath stageBit = do
  putStrLn $ show stageBit ++ ": " ++ shaderFilePath
  (codeSize, codePtr) <- compileGLSL shaderFilePath
  shaderModuleCreateInfo <- getShaderModuleCreateInfo codeSize codePtr
  shaderModule <- alloca $ \shaderModulePtr -> do
    result <- vkCreateShaderModule device (unsafePtr shaderModuleCreateInfo) VK_NULL shaderModulePtr
    validationVK result "vkCreateShaderModule failed!"
    peek shaderModulePtr
  touchVkData shaderModuleCreateInfo
  free codePtr
  return $ getShaderCreateInfo stageBit shaderModule

destroyShaderStageCreateInfo :: VkDevice -> VkPipelineShaderStageCreateInfo -> IO ()
destroyShaderStageCreateInfo device shaderStageCreateInfo = do
  vkDestroyShaderModule device (getField @"module" shaderStageCreateInfo) VK_NULL
  touchVkData shaderStageCreateInfo
