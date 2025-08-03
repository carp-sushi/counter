module Logger where

import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr)
import Control.Monad.Logger.CallStack (defaultOutput)
import System.IO (stdout)

-- | Log function type.
type LogFunc =
    Loc ->
    LogSource ->
    LogLevel ->
    LogStr ->
    IO ()

-- | Create a log function that writes to stdout.
stdoutLogging :: LogFunc
stdoutLogging =
    defaultOutput stdout

-- | Create a log function that does nothing.
noLogging :: LogFunc
noLogging _ _ _ _ =
    return ()
