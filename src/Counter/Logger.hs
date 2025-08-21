module Counter.Logger where

import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr)
import Control.Monad.Logger.CallStack (defaultOutput)
import System.IO (stdout)

-- | Log function type.
type LogFn =
    Loc ->
    LogSource ->
    LogLevel ->
    LogStr ->
    IO ()

-- | Create a log function that writes to stdout.
stdoutLogging :: LogFn
stdoutLogging =
    defaultOutput stdout

-- | Create a log function that does nothing.
noLogging :: LogFn
noLogging _ _ _ _ =
    pure ()
