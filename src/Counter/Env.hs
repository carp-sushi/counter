{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Counter.Env where

import Counter.Domain (Incrementer (incrementCount), Querier (queryCount))
import Counter.Logger (LogFn)
import Counter.Repo (CounterRepo (..))

import Control.Monad (when)
import Control.Monad.Logger (LogLevel, MonadLogger (..), toLogStr)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, liftIO, runReaderT)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

-- | App environment
data Env = Env
    { envCounterRepo :: !CounterRepo
    , envLogFn :: !LogFn
    , envLogLevel :: !LogLevel
    }

-- | Custom reader monad stack for request handlers.
newtype AppT m a = AppT {unAppT :: ReaderT Env m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader Env
        , MonadIO
        )

-- | Run an AppT monad with a given environment.
runAppT :: Env -> AppT m a -> m a
runAppT env appt =
    runReaderT (unAppT appt) env

-- | MonadLogger instance for AppT.
instance (MonadIO m) => MonadLogger (AppT m) where
    monadLoggerLog loc src lvl msg = do
        logLevel <- asks envLogLevel
        when (lvl >= logLevel) $ do
            ts <- liftIO getCurrentTime
            let tsFmt = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" ts
                timestamp = toLogStr $ "[" <> tsFmt <> "] "
            logFn <- asks envLogFn
            liftIO $ logFn loc src lvl (timestamp <> toLogStr msg)

-- | Incrementer instance for AppT.
instance (MonadIO m) => Incrementer (AppT m) where
    incrementCount key value = do
        CounterRepo{..} <- asks envCounterRepo
        liftIO $ counterRepoIncrement key value

-- | Querier instance for AppT.
instance (MonadIO m) => Querier (AppT m) where
    queryCount key = do
        CounterRepo{..} <- asks envCounterRepo
        liftIO $ counterRepoQuery key
