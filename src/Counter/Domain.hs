module Counter.Domain where

import Data.Aeson hiding (Key)
import Data.Text (Text)

-- Counter primitives.
type Key = Text
type Count = Integer

-- | Counter domain object.
data Counter = Counter !Key !Count
    deriving (Eq, Ord, Show)

-- Render counter as JSON.
instance ToJSON Counter where
    toJSON (Counter key count) =
        object ["key" .= key, "count" .= count]

-- | Increment a counter.
class (Monad m) => Incrementer m where
    incrementCount :: Key -> Count -> m Counter

-- | Query a counter.
class (Monad m) => Querier m where
    queryCount :: Key -> m Counter
