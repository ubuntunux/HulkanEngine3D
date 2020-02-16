{-# LANGUAGE Strict               #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE UnboxedTuples       #-}

module HulkanEngine3D.Utilities.System
    ( VulkanException (..)
    , getSystemTime
    , handleAllErrors
    , validationVK
    , throwingVK
    , throwVKMsg
    , withCStringList
    , asListVK
    , withVkArrayLen
    , unsafeAddr
    , unsafeToPtr
    , ptrAtIndex
    , allocaPeek
    , allocaPeekArray
    ) where

import GHC.Base
import GHC.Exts
import Control.Exception
import Control.Monad (when)
import Data.IORef
import qualified Data.Time.Clock.System as SystemTime

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

-- | Use this to throw all exceptions in this project
data VulkanException
  = VulkanException
  { vkeCode    :: Maybe VkResult
  , vkeMessage :: String
  } deriving (Eq, Show, Read)

instance Exception VulkanException where
  displayException (VulkanException Nothing msg)
    = unlines
    [ ""
    , "Vulkan exception:"
    , "*** " ++ msg
    ]
  displayException (VulkanException (Just c) msg)
    = unlines
    [ ""
    , "Vulkan error: " ++ show c
    , "*** " ++ msg
    ]

instance Show (IORef a) where
  show _ = "<ioref>"


-- | Low latency time in seconds since the start
getSystemTime :: IO Double
getSystemTime = do
    now <- SystemTime.getSystemTime
    let seconds = SystemTime.systemSeconds now
        -- Have to nanoseconds convert from Word64 before subtraction to allow negative delta.
        nanoseconds :: Int64 = fromIntegral (SystemTime.systemNanoseconds now)
        -- Seconds in Double keep at least microsecond-precision for 285 years.
        -- Float is not good enough even for millisecond-precision over more than a few hours.
        result :: Double = fromIntegral seconds + fromIntegral nanoseconds / 1e9
    return result

-- | Handle any error and return default value
handleAllErrors :: a -> SomeException -> IO a
handleAllErrors a (SomeException e)
  = a <$ putStrLn (displayException e)

-- | Throw VulkanException if something goes wrong
validationVK :: VkResult
             -> String
             -> IO ()
validationVK result msg = do
  when (result < VK_SUCCESS) $ throwIO $ VulkanException (Just result) msg

throwingVK :: String
           -> IO VkResult
           -> IO ()
throwingVK msg f = do
  vkRez <- f
  validationVK vkRez msg

-- | Throw VulkanException without error code
throwVKMsg :: String -> IO a
throwVKMsg msg = throwIO $ VulkanException Nothing msg

-- | Use list of haskell strings as @Ptr CString@
withCStringList :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStringList [] f = f 0 nullPtr
withCStringList xs f = go xs [] 0
  where
    go [] pts n     = withArray (reverse pts) (f n)
    go (s:ss) pts n = withCString s (\p -> go ss (p:pts) (n+1))

-- | Get size of action output and then get the result,
--   performing data copy.
asListVK :: Storable x
         => (Ptr Word32 -> Ptr x -> IO ())
         -> IO [x]
asListVK action = alloca $ \counterPtr -> do
  action counterPtr VK_NULL_HANDLE
  counter <- fromIntegral <$> peek counterPtr
  if counter <= 0
  then pure []
  else allocaArray counter $ \valPtr -> do
    action counterPtr valPtr
    peekArray counter valPtr

-- | Prevent earlier GC of given value
touch :: a -> IO ()
touch x = GHC.Base.IO $ \s -> case GHC.Base.touch# x s of s' -> (# s', () #)
{-# INLINE touch #-}

-- | This should probably be in Graphics.Vulkan.Marshal
withVkArrayLen :: (Storable a, VulkanMarshal a) => [a] -> (Word32 -> Ptr a -> IO b) -> IO b
withVkArrayLen xs pf = do
  ret <- withArrayLen xs (pf . fromIntegral)
  touch xs
  return ret
{-# INLINE withVkArrayLen #-}

-- Any is a type to which any type can be safely unsafeCoerced to.
aToWord# :: Any -> Word#
aToWord# a = let !mb = a in case unsafeCoerce# mb :: Word of W# addr -> addr

unsafeAddr :: a -> Int
unsafeAddr a = I# (word2Int# (aToWord# (unsafeCoerce# a)))

unsafeToPtr  :: forall a. Storable a => a -> Ptr a
unsafeToPtr a = Ptr (unsafeCoerce# a)
{-# INLINE unsafeToPtr #-}

ptrAtIndex :: forall a. Storable a => Ptr a -> Int -> Ptr a
ptrAtIndex ptr i = ptr `plusPtr` (i * sizeOf @a undefined)

allocaPeek :: Storable a => (Ptr a -> IO b) -> IO a
allocaPeek action = alloca $ \ptr -> do
  action ptr >> peek ptr

allocaPeekArray :: Storable a => Int -> (Ptr a -> IO b) -> IO [a]
allocaPeekArray count action = allocaArray count $ \arrayPtr -> do
   action arrayPtr
   peekArray count arrayPtr