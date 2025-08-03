module Database (
    defaultConnection,
    newCounterService,
) where

import Domain
import Service

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as TE
import Database.Redis
import Text.Read (readMaybe)

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
    let k = TE.encodeUtf8 key
        v = fromIntegral value
     in runRedis conn $ void $ incrby k v

-- | Query the value of a counter in Redis.
redisCounterQuery :: Connection -> Key -> IO Count
redisCounterQuery conn key =
    runRedis conn $ do
        value <- get $ TE.encodeUtf8 key
        return $ readCount value

-- Read count from a redis result.
readCount :: Either Reply (Maybe ByteString) -> Count
readCount value =
    case value of
        Right (Just v) -> readSafe $ cs v
        _ -> 0

-- Read count from a string.
readSafe :: String -> Count
readSafe s =
    case readMaybe s of
        Just n -> n
        Nothing -> 0
