module Service where

import Domain

-- | Counter service is a record that contains functions to manage counters.
data CounterService m = CounterService
    { counterServiceIncrement :: Key -> Count -> m ()
    , counterServiceQuery :: Key -> m Count
    }
