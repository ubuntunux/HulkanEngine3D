{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module HulkanEngine3D.Vulkan.Shader
  ( createShaderStageCreateInfo
  , destroyShaderStageCreateInfo  
  , compileGLSL
  )  where

import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Time.Clock
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

import HulkanEngine3D.Utilities.System
import HulkanEngine3D.Utilities.Logger

materialDirectory :: FilePath
materialDirectory = "Resource/Materials"

shaderDirectory :: FilePath
shaderDirectory = "Resource/Shaders"


spirvFilePathWithDefines :: String -> [Text.Text] -> FilePath
spirvFilePathWithDefines shaderFileName shaderDefines =
    let (justFilePath, ext) = splitExtension shaderFileName
        shaderFilePostFix = Text.unpack . Text.replace (Text.pack "=") (Text.pack "") . Text.replace (Text.pack " ") (Text.pack "_") . Text.unwords $ shaderDefines
    in
        materialDirectory </> (if null shaderFilePostFix then justFilePath else justFilePath ++ "_" ++ shaderFilePostFix) ++ ext ++ ".spirv"


compileGLSL :: FilePath -> [Text.Text] -> IO (Int, Ptr Word32)
compileGLSL shaderFileName shaderDefines = do
    validatorExe <- fromMaybe
        ( error $ unlines
          [ "Cannot find glslangValidator executable."
          , "Check if it is available in your $PATH."
          , "Read more about it at https://www.khronos.org/opengles/sdk/tools/Reference-Compiler/"]
        ) <$> findExecutable "glslangValidator"
    tmpDir <- getTemporaryDirectory
    curDir <- getCurrentDirectory
    createDirectoryIfMissing True tmpDir
    let shaderDefineArgs = Text.unpack . Text.unwords . map (Text.append $ Text.pack "-D") $ shaderDefines
        shaderFilePath = shaderDirectory </> shaderFileName
        shaderDir = takeDirectory shaderFilePath
        spirvCodeFile = spirvFilePathWithDefines shaderFileName shaderDefines
        forceConvert = True -- need recursive include file time diff implementation
    -- convert glsl -> spirv
    doConvert shaderFilePath spirvCodeFile >>= \result ->
        when (result || forceConvert) $ do
            createDirectoryIfMissing True (takeDirectory spirvCodeFile)
            doesFileExist shaderFilePath >>= flip unless (error $ "compileGLSL: " ++ shaderFilePath ++ " does not exist.")
            (exitCode, stdo, stde) <- readCreateProcessWithExitCode
                ((shell $ validatorExe ++ " -V -o " ++ spirvCodeFile ++ " " ++ shaderFilePath ++ " " ++ shaderDefineArgs) { cwd = Just "." }) ""
            case exitCode of
                ExitSuccess -> pure ()
                ExitFailure i -> do
                    logInfo stdo
                    logInfo stde
                    error $ "glslangValidator exited with code " ++ show i ++ "."

    -- read spirv
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
    where
        -- TODO : need recursive include file time diff implementation
        doConvert shaderFilePath spirvFilePath = do
            doesFileExist spirvFilePath >>= \case
                True -> do
                    shaderFileTime <- getModificationTime shaderFilePath
                    spirvFileTime <- getModificationTime spirvFilePath
                    return $ (0::NominalDiffTime) < diffUTCTime shaderFileTime spirvFileTime
                False -> return True

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

createShaderStageCreateInfo :: VkDevice -> String -> [Text.Text] -> VkShaderStageFlagBits -> IO VkPipelineShaderStageCreateInfo
createShaderStageCreateInfo device shaderFileName shaderDefines stageBit = do
    logInfo $ "createShaderStageCreateInfo : " ++ show stageBit ++ ": " ++ shaderFileName
    -- ex) shaderDefines = ["STATIC_MESH", "RENDER_SHADOW=true", "SAMPLES=16"]
    (codeSize, codePtr) <- compileGLSL shaderFileName []
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
    let shaderModule = getField @"module" shaderStageCreateInfo
        shaderStage = getField @"stage" shaderStageCreateInfo
    logInfo $ "destroyShaderStageCreateInfo : stage " ++ show shaderStage ++ ", module " ++ show shaderModule
    vkDestroyShaderModule device (getField @"module" shaderStageCreateInfo) VK_NULL
    touchVkData shaderStageCreateInfo
