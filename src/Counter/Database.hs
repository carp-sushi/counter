module Counter.Database (
    defaultConnection,
    newCounterRepo,
) where

import Counter.Domain (Count, Counter (Counter), Key)
import Counter.Repo (CounterRepo (..))

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as TE
import Database.Redis (Connection, checkedConnect, defaultConnectInfo, get, incrby, runRedis)
import Text.Read (readMaybe)

-- | Create a new Redis connection using default settings.
defaultConnection :: IO Connection
defaultConnection =
    checkedConnect defaultConnectInfo

-- | Create a new counter service backed by Redis.
newCounterRepo :: Connection -> CounterRepo
newCounterRepo conn =
    CounterRepo
        { counterRepoIncrement = redisCounterIncrement conn
        , counterRepoQuery = redisCounterQuery conn
        }

-- Increment a counter in Redis.
redisCounterIncrement :: Connection -> Key -> Count -> IO Counter
redisCounterIncrement conn key value = do
    result <- runRedis conn $ incrby (TE.encodeUtf8 key) value
    pure $ Counter key $ readIntegerCount result

-- Read count from a redis integer result.
readIntegerCount :: Either r Integer -> Count
readIntegerCount (Right count) = count
readIntegerCount (Left _) = 0

-- Query the value of a counter in Redis.
redisCounterQuery :: Connection -> Key -> IO Counter
redisCounterQuery conn key =
    runRedis conn $ do
        value <- get $ TE.encodeUtf8 key
        pure $ Counter key $ readByteStringCount value

-- Read count from a redis byte string result.
readByteStringCount :: Either r (Maybe ByteString) -> Count
readByteStringCount value =
    case value of
        Right (Just v) -> fromMaybe 0 $ readMaybe $ cs v
        _ -> 0
