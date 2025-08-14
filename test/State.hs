module State (
    State,
    newState,
    newCounterService,
) where

import Counter.Domain
import Counter.Service

import Control.Concurrent.MVar
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

-- | In-memory storage type for testing.
type State = MVar (HashMap Key Count)

-- | Create an empty state.
newState :: IO State
newState =
    newMVar HM.empty

-- | Create a new counter test service.
newCounterService :: State -> CounterService
newCounterService state =
    CounterService
        { counterServiceIncrement = stateCounterIncrement state
        , counterServiceQuery = stateCounterQuery state
        }

-- | Increment a counter under a key.
stateCounterIncrement :: State -> Key -> Count -> IO Counter
stateCounterIncrement state key value = do
    modifyMVar_ state $ return . HM.insertWith (+) key value
    stateCounterQuery state key

-- | Query a counter by key.
stateCounterQuery :: State -> Key -> IO Counter
stateCounterQuery state key = do
    counters <- readMVar state
    return $ Counter key $ HM.findWithDefault 0 key counters
