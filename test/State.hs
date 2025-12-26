module State (
    State,
    newState,
    stateCounterRepo,
) where

import Counter.Domain (Count, Counter (..), Key)
import Counter.Repo (CounterRepo (..))

import qualified Control.Monad.STM as STM
import Data.Maybe (fromMaybe)
import StmContainers.Map (Map)
import qualified StmContainers.Map as HM

-- | In-memory storage type for testing.
type State = Map Key Count

-- | Create an empty state.
newState :: IO State
newState =
    HM.newIO

-- | Create a new fake counter repository backed by in-memory state.
stateCounterRepo :: State -> CounterRepo
stateCounterRepo state =
    CounterRepo
        { counterRepoIncrement = stateCounterIncrement state
        , counterRepoQuery = stateCounterQuery state
        }

-- | Increment a counter under a key.
stateCounterIncrement :: State -> Key -> Count -> IO Counter
stateCounterIncrement state key value = do
    STM.atomically $ do
        maybeCount <- HM.lookup key state
        let newCount = maybe value (+ value) maybeCount
        HM.insert newCount key state
        pure $ Counter key newCount

-- | Query a counter by key.
stateCounterQuery :: State -> Key -> IO Counter
stateCounterQuery state key = do
    STM.atomically $ do
        maybeCount <- HM.lookup key state
        pure $ Counter key $ fromMaybe 0 maybeCount
