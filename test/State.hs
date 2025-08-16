module State (
    State,
    newState,
    fakeCounterRepo,
) where

import Counter.Domain
import Counter.Repo

import Control.Concurrent.MVar
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

-- | In-memory storage type for testing.
type State = MVar (HashMap Key Count)

-- | Create an empty state.
newState :: IO State
newState =
    newMVar HM.empty

-- | Create a new fake counter repository backed by in-memory state.
fakeCounterRepo :: State -> CounterRepo
fakeCounterRepo state =
    CounterRepo
        { counterRepoIncrement = stateCounterIncrement state
        , counterRepoQuery = stateCounterQuery state
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
