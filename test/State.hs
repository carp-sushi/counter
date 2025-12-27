module State (
    State,
    newState,
    stateCounterRepo,
) where

import Counter.Domain (Count, Counter (..), Key)
import Counter.Repo (CounterRepo (..))

import Control.Monad.STM (atomically)
import Data.Maybe (fromMaybe)
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map

-- | In-memory storage type for testing.
type State = Map Key Count

-- | Create an empty state.
newState :: IO State
newState =
    Map.newIO

-- | Create a new fake counter repository backed by in-memory state.
stateCounterRepo :: State -> CounterRepo
stateCounterRepo state =
    CounterRepo
        { counterRepoIncrement = stateCounterIncrement state
        , counterRepoQuery = stateCounterQuery state
        }

-- | Increment a counter under a key.
stateCounterIncrement :: State -> Key -> Count -> IO Counter
stateCounterIncrement state key value =
    atomically $ do
        maybeCount <- Map.lookup key state
        let newCount = maybe value (+ value) maybeCount
        Map.insert newCount key state
        pure $ Counter key newCount

-- | Query a counter by key.
stateCounterQuery :: State -> Key -> IO Counter
stateCounterQuery state key =
    atomically $ do
        maybeCount <- Map.lookup key state
        pure $ Counter key $ fromMaybe 0 maybeCount
