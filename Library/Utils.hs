{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE Strict                #-}

module Library.Utils
    ( handleAllErrors
    , throwingVK
    , throwVKMsg
    , withCStringList
    , asListVK
    , Program (..), Program', runProgram
    , MonadIO (..)
    , allocResource, allocResource', locally
    , ProgramState (..), MonadState (..), modify, modify', gets
    , VulkanException (..), MonadError (..), runVk
    , try, bracket, finally, throwVkMsg
    , logDebug, logInfo, logWarn, logError
    , isDev, inDev
    , liftIOWith, withVkPtr
    , getTime
    , LoopControl (..)
    , checkStatus
    , occupyThreadAndFork
    , loop
    ) where

import qualified Control.Exception as Exception
import Control.Exception
  ( Exception
  , throwIO
  , displayException
  , throwTo)
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Control.Monad.Logger           as Logger
import qualified Control.Monad.Logger.CallStack as LoggerCS
import Control.Monad.State.Class
import Data.IORef
import Data.String (fromString)
import Data.Time.Clock.System
import Data.Tuple (swap)
import GHC.Stack
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import System.Exit


data ProgramState
  = ProgramState
  { currentStatus :: VkResult
    -- ^ Result of the last vulkan command.
    --   We may need it to check if result is some non-zero non-error code.
  , loggingFunc   :: Logger.Loc
                  -> Logger.LogSource
                  -> Logger.LogLevel
                  -> Logger.LogStr -> IO ()
    -- ^ Enable monad-logger.
  , startTime :: SystemTime
    -- ^ Time for animations and physics
  }

iProgState :: IO ProgramState
iProgState = do
  -- get logger function from Control.Monad.Logger transformer
  logFun <- Logger.runStdoutLoggingT $ Logger.LoggingT pure
  time <- getSystemTime
  return ProgramState
    { currentStatus = VK_SUCCESS
    , loggingFunc   = logFun
    , startTime     = time
    }

-- | Program is modelled as a combination of several transformers:
--
--   * ReaderT + IORef to model state
--   * ContT for exception handling and careful resource management
--   * ExceptT for exception handling
newtype Program r a = Program
  { unProgram :: IORef ProgramState
              -> (Either VulkanException a -> IO r) -> IO r }

-- | A common case when program parameter @r@ is restricted to be
--   either conter of the monad or an error.
type Program' a = Program (Either VulkanException a) a


runProgram :: (Either VulkanException a -> IO r)
           -> Program r a -> IO r
runProgram c p = iProgState >>= newIORef >>= flip (unProgram p) c


-- | Allocate some resource and return it,
--   free that resource after the continuation is executed.
--
--   If exception happens during allocation, the freeing does not happen.
--
allocResource :: (a -> Program' ()) -- ^ free resource
              -> Program r a -- ^ allocate resource
              -> Program r a
allocResource free alloc = Program $ \ref c ->
  unProgram alloc ref $ \case
    Left e -> c (Left e)
    Right a -> c (Right a) >>= \r -> r <$ unProgram (free a) ref pure
{-# INLINE allocResource #-}

-- | The same as `allocResource`, but does not prepend a
--   resource release action to the continuation.
--   Instead, return the resource-release-action-continuation-prepending action.
--   The latter may be envoked later to add the resource-release-action to
--   the beginning of the continuation.
--
--   This gives a more granular control over when the release of a resource
--   should happen. For example, when the resource allocation and release
--   should happen in the same order.
allocResource' :: (a -> Program' ()) -- ^ free resource
               -> Program r a -- ^ allocate resource
               -> Program r (a, Program r ())
allocResource' free alloc = Program $ \ref c ->
  unProgram alloc ref $ \case
    Left e -> c (Left e)
    Right a -> c (Right (a, Program $ \ref' c' ->
                                 c' (Right ()) >>=
                                      \r -> r <$ unProgram (free a) ref' pure))
{-# INLINE allocResource' #-}

-- | Run nested continuations locally:
--     fully execute the program in IO;
--     all allocated resources are released before this function returns.
--
--   Note: this means none of the resources allocated in the nested program
--         can be used outside of it! Never return them from `locally`!
locally :: Program' a
        -> Program r a
locally p = Program $ \ref c -> unProgram p ref pure >>= c
{-# INLINE locally #-}

instance Functor (Program r) where
  fmap f p = Program $ \ref c -> unProgram p ref (c . fmap f)
  {-# INLINE fmap #-}

instance Applicative (Program r) where
  pure x = Program $ const ($ Right x)
  {-# INLINE pure #-}
  pf <*> px = Program $
    \ref c -> unProgram pf ref $ \g -> unProgram px ref (c . (g <*>) )
  {-# INLINE (<*>) #-}

instance Monad (Program r) where
  return = pure
  {-# INLINE return #-}
  px >>= k = Program $
    \ref c -> unProgram px ref $ \case
      Right x -> unProgram (k x) ref c
      Left e  -> c (Left e)
  {-# INLINE (>>=) #-}

instance MonadIO (Program r) where
  liftIO m = Program $ const (Right <$> m >>=)
  {-# INLINE liftIO #-}

instance MonadState ProgramState (Program r) where
  get = Program $ \ref -> (Right <$> readIORef ref >>=)
  {-# INLINE get #-}
  put s = Program $ \ref -> (Right <$> writeIORef ref s >>=)
  {-# INLINE put #-}
  state f = Program $ \ref -> (Right <$> atomicModifyIORef' ref (swap . f) >>=)
  {-# INLINE state #-}


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

instance MonadError VulkanException (Program r) where
  throwError e = Program $ const ($ Left e)
  {-# INLINE throwError #-}
  catchError px catcher = Program $ \ref c -> unProgram px ref $ \case
    Left e  -> unProgram (catcher e) ref c
    Right r -> c (Right r)
  {-# INLINE catchError #-}

-- | Handle any error and return default value
handleAllErrors :: a -> Exception.SomeException -> IO a
handleAllErrors a (Exception.SomeException e)
  = a <$ putStrLn (displayException e)

-- | Throw VulkanException if something goes wrong
throwingVK :: String -> IO VkResult -> IO ()
throwingVK msg f = do
  vkRez <- f
  when (vkRez < VK_SUCCESS) $ throwIO $ VulkanException (Just vkRez) msg

-- | Throw VulkanException without error code
throwVKMsg :: String -> IO a
throwVKMsg msg = throwIO $ VulkanException Nothing msg

-- | Throw VulkanException without error code
throwVkMsg :: HasCallStack => String -> Program r a
throwVkMsg msg = throwError . VulkanException Nothing $ unlines
  [ msg, prettyCallStack callStack ]
{-# INLINE throwVkMsg #-}

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


-- | An adaptation of @bracket@ from `Control.Exception`.
--
--   This does not handle vanilla Haskell exceptions, and should only be used
--   to catch the exception defined in this module.
bracket :: Program r a        -- ^ computation to run first (\"acquire resource\")
        -> (a -> Program r b) -- ^ computation to run last (\"release resource\")
        -> (a -> Program r c) -- ^ computation to run in-between
        -> Program r c        -- returns the value from the in-between computation
bracket before after thing = do
  a <- before
  er <- try (thing a)
  _ <- after a
  Program $ const ($ er)
{-# INLINE bracket #-}


-- | A specialised variant of 'bracket' with just a computation to run
--   afterward.
--
--  An adaptation of @finally@ from `Control.Exception`
--
--   This does not handle vanilla Haskell exceptions, and should only be used
--   to catch the exception defined in this module.
finally :: Program r a  -- ^ computation to run first
        -> Program r b  -- ^ computation to run afterward (even if an exception
                        -- was raised)
        -> Program r a  -- returns the value from the first computation
finally a sequel = do
  er <- try a
  _ <- sequel
  Program $ const ($ er)
{-# INLINE finally #-}


-- | An adaptation of @try@ from `Control.Exception`
--
--   This does not handle vanilla Haskell exceptions, and should only be used
--   to catch the exception defined in this module.
try :: Program r a -> Program r (Either VulkanException a)
try a = Program $ \ref c -> unProgram a ref $ c . Right
{-# INLINE try #-}


-- | Run vulkan command, throwing an exception if its result is an error.
runVk :: HasCallStack => IO VkResult -> Program r ()
runVk action = do
  r <- liftIO action
  state $ \s -> ((), s { currentStatus = r })
  when (r < VK_SUCCESS) . throwError . VulkanException (Just r)
    $ "Vulkan command returned an error VkResult\n"
    ++ prettyCallStack callStack
{-# INLINE runVk #-}


instance Logger.MonadLogger (Program r) where
  monadLoggerLog loc ls ll msg = do
    logFun <- gets loggingFunc
    liftIO $ logFun loc ls ll (Logger.toLogStr msg)
  {-# INLINE monadLoggerLog #-}


logDebug :: HasCallStack => String -> Program r ()
#ifdef DEVELOPMENT
logDebug = LoggerCS.logDebug . fromString
#else
logDebug = const (pure ())
#endif
{-# INLINE logDebug #-}

logInfo :: HasCallStack => String -> Program r ()
logInfo = LoggerCS.logInfo . fromString
{-# INLINE logInfo #-}

logWarn :: HasCallStack => String -> Program r ()
logWarn = LoggerCS.logWarn . fromString
{-# INLINE logWarn #-}

logError :: HasCallStack => String -> Program r ()
logError = LoggerCS.logError . fromString
{-# INLINE logError #-}


isDev :: Bool
#ifdef DEVELOPMENT
isDev = True
#else
isDev = False
#endif
{-# INLINE isDev #-}

inDev :: Applicative m => m () -> m ()
#ifdef DEVELOPMENT
inDev = id
#else
inDev = const (pure ())
#endif
{-# INLINE inDev #-}


-- | Run an IO action with a callback in Program monad
liftIOWith :: ((a -> IO (Either VulkanException b))
                    -> IO (Either VulkanException b)
              )
           -> (a -> Program' b) -> Program r b
liftIOWith iof pf = Program $ \ref c ->
  iof (\a -> unProgram (pf a) ref pure) >>= c
{-# INLINE liftIOWith #-}


withVkPtr :: VulkanMarshal a
          => a
          -> (Ptr a -> Program' b)
          -> Program r b
withVkPtr x = liftIOWith (withPtr x)
{-# INLINE withVkPtr #-}

-- | Low latency time in seconds since the start
getTime :: Program r Double
getTime = do
    now <- liftIO getSystemTime
    start <- startTime <$> get
    let deltaSeconds = systemSeconds now - systemSeconds start
        -- Have to nanoseconds convert from Word64 before subtraction to allow negative delta.
        deltaNanoseconds :: Int64 = fromIntegral (systemNanoseconds now) - fromIntegral (systemNanoseconds start)
        -- Seconds in Double keep at least microsecond-precision for 285 years.
        -- Float is not good enough even for millisecond-precision over more than a few hours.
        seconds :: Double = fromIntegral deltaSeconds + fromIntegral deltaNanoseconds / 1e9
    return seconds


data LoopControl = ContinueLoop | AbortLoop deriving Eq


checkStatus :: Either VulkanException () -> IO ()
checkStatus (Right ()) = pure ()
checkStatus (Left err) = do
  putStrLn $ displayException err
  exitFailure


-- | For C functions that have to run in the main thread as long as the program runs.
--
--   Caveat: The separate thread is not a bound thread, in contrast to the main thread.
--   Use `runInBoundThread` there if you need thread local state for C libs.
occupyThreadAndFork :: Program r () -- ^ the program to run in the main thread
                    -> Program' () -- ^ the program to run in a separate thread
                    -> Program r ()
occupyThreadAndFork mainProg deputyProg = Program $ \ref c -> do
  mainThreadId <- myThreadId
  threadRef <- newIORef =<< readIORef ref
  _ <- Control.Concurrent.forkFinally (unProgram deputyProg threadRef pure >>= checkStatus) $ \case
    Left exception -> throwTo mainThreadId exception
    Right ()       -> throwTo mainThreadId ExitSuccess
  unProgram mainProg ref c


loop :: Program' LoopControl -> Program r ()
loop action = do
  status <- locally action
  if status == ContinueLoop then loop action else return ()