module Counter.Database (
    defaultConnection,
    newCounterService,
) where

import Counter.Domain
import Counter.Service

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as TE
import Database.Redis
import Text.Read (readMaybe)

-- | Create a new Redis connection using default settings.
defaultConnection :: IO Connection
defaultConnection =
    checkedConnect defaultConnectInfo

-- | Create a new counter service backed by Redis.
newCounterService :: Connection -> CounterService
newCounterService conn =
    CounterService
        { counterServiceIncrement = redisCounterIncrement conn
        , counterServiceQuery = redisCounterQuery conn
        }

-- Increment a counter in Redis.
redisCounterIncrement :: Connection -> Key -> Count -> IO Counter
redisCounterIncrement conn key value = do
    result <- runRedis conn $ incrby k v
    return $ Counter key $ readIntegerCount result
  where
    k = TE.encodeUtf8 key
    v = fromIntegral value

-- Read count from a redis integer result.
readIntegerCount :: Either Reply Integer -> Count
readIntegerCount (Right count) = count
readIntegerCount (Left _) = 0

-- Query the value of a counter in Redis.
redisCounterQuery :: Connection -> Key -> IO Counter
redisCounterQuery conn key =
    runRedis conn $ do
        value <- get $ TE.encodeUtf8 key
        return $ Counter key $ readByteStringCount value

-- Read count from a redis byte string result.
readByteStringCount :: Either Reply (Maybe ByteString) -> Count
readByteStringCount value =
    case value of
        Right (Just v) -> fromMaybe 0 $ readMaybe $ cs v
        _ -> 0
