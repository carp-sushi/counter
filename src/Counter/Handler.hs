module Counter.Handler (
    getStatusH,
    postCountersH,
    deleteCountersH,
    getCountersH,
) where

import Counter.Domain
import Counter.Env

import Control.Monad.Logger (MonadLogger, logDebugN)
import Control.Monad.Reader (MonadIO)
import Data.Text (Text)

-- | Handler for getting the server status.
getStatusH :: (MonadIO m) => AppT m Text
getStatusH = return "up"

-- | Handler for incrementing a counter.
postCountersH :: (MonadLogger m, Incrementer m) => Key -> m Counter
postCountersH key = do
    logDebugN $ "increment counter: " <> key
    incrementCount key 1

-- | Handler for decrementing a counter.
deleteCountersH :: (MonadLogger m, Incrementer m) => Key -> m Counter
deleteCountersH key = do
    logDebugN $ "decrement counter: " <> key
    incrementCount key (-1)

-- | Handler for getting the count for a key.
getCountersH :: (MonadLogger m, Querier m) => Key -> m Counter
getCountersH key = do
    logDebugN $ "query counter: " <> key
    queryCount key
