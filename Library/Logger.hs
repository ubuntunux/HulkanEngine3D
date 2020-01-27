{-# LANGUAGE RecordWildCards #-}

module Library.Logger
    ( logInfo
    , logDebug
    , logWarn
    , logError
    ) where

import GHC.Stack hiding (prettyCallStack, prettySrcLoc)
import Data.List

prettySrcLoc :: SrcLoc -> String
prettySrcLoc SrcLoc {..}
  = foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine, ":"
      , show srcLocStartCol, " in "
      , srcLocPackage, ":", srcLocModule
      ]


prettyCallStack :: CallStack -> String -> String -> String
prettyCallStack cs loggerLevel msg = intercalate "\n" $ prettyCallStackLines cs loggerLevel msg


prettyCallStackLines :: CallStack -> String -> String -> [String]
prettyCallStackLines cs loggerLevel msg = case getCallStack cs of
  []  -> []
  stk -> map ((++"    ") . prettyCallSite) stk
  where
    prettyCallSite (f, loc) = "[" ++ loggerLevel ++ "] " ++ msg ++ " (" ++ prettySrcLoc loc ++ ")"


logInfo :: HasCallStack => String -> IO ()
logInfo msg = do
    putStrLn $ prettyCallStack callStack "INFO" msg

logDebug :: HasCallStack => String -> IO ()
logDebug msg = do
    putStrLn $ prettyCallStack callStack "DEBUG" msg

logWarn :: HasCallStack => String -> IO ()
logWarn msg = do
    putStrLn $ prettyCallStack callStack "WARNING" msg

logError :: HasCallStack => String -> IO ()
logError msg = do
    putStrLn $ prettyCallStack callStack "ERROR" msg