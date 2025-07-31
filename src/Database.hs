module Database (
    defaultConnection,
    newCounterService,
) where

import Domain
import Service

import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as TE
import Database.Redis

-- | Create a new Redis connection using default settings.
defaultConnection :: IO Connection
defaultConnection =
    checkedConnect defaultConnectInfo

-- | Create a new counter service backed by Redis.
newCounterService :: Connection -> CounterService IO
newCounterService conn =
    CounterService
        { counterServiceIncrement = redisCounterIncrement conn
        , counterServiceQuery = redisCounterQuery conn
        }

-- | Increment a counter in Redis.
redisCounterIncrement :: Connection -> Key -> Count -> IO ()
redisCounterIncrement conn key value =
    runRedis conn $ do
        _ <- incrby (TE.encodeUtf8 key) (fromIntegral value)
        return ()

-- | Query the value of a counter in Redis.
redisCounterQuery :: Connection -> Key -> IO Count
redisCounterQuery conn key =
    runRedis conn $ do
        value <- get (TE.encodeUtf8 key)
        return $ case value of
            Right (Just v) -> (read (cs v) :: Count)
            _ -> 0
