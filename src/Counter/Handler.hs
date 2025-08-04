module Counter.Handler (
    getStatusH,
    postCountersH,
    deleteCountersH,
    getCountersH,
) where

import Counter.Domain
import Counter.Env

import Control.Monad.Logger (logDebugN)
import Control.Monad.Reader (MonadIO)
import Data.Text (Text)
import Servant

-- | Handler for getting the server status.
getStatusH :: (MonadIO m) => AppT m Text
getStatusH = return "up"

-- | Handler for incrementing a counter.
postCountersH :: (MonadIO m) => Key -> AppT m NoContent
postCountersH key = do
    logDebugN $ "increment counter: " <> key
    incrementCount key 1
    return NoContent

-- | Handler for decrementing a counter.
deleteCountersH :: (MonadIO m) => Key -> AppT m NoContent
deleteCountersH key = do
    logDebugN $ "decrement counter: " <> key
    incrementCount key (-1)
    return NoContent

-- | Handler for getting the count for a key.
getCountersH :: (MonadIO m) => Key -> AppT m Counter
getCountersH key = do
    logDebugN $ "query counter: " <> key
    count <- queryCount key
    return $ Counter key count
