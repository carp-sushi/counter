{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env where

import Domain
import Logger
import Service

import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger (..), toLogStr)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, liftIO)

-- | App environment
data Env = Env
    { envCounterService :: !(CounterService IO)
    , envLogFunc :: !LogFunc
    }

-- | Custom reader monad stack for request handlers.
newtype AppT m a = AppT {unAppT :: ReaderT Env m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader Env
        , MonadIO
        , MonadError err
        )

-- | MonadLogger instance for AppT.
instance (MonadIO m) => MonadLogger (AppT m) where
    monadLoggerLog loc src lvl msg = do
        logFunc <- asks envLogFunc
        liftIO $ logFunc loc src lvl (toLogStr msg)

-- | Incrementer instance for AppT.
instance (MonadIO m) => Incrementer (AppT m) where
    incrementCounter key value = do
        counterService <- asks envCounterService
        liftIO $ counterServiceIncrement counterService key value

-- | Querier instance for AppT.
instance (MonadIO m) => Querier (AppT m) where
    queryCounter key = do
        counterService <- asks envCounterService
        liftIO $ counterServiceQuery counterService key
