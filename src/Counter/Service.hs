module Counter.Service where

import Counter.Domain

-- | Counter service is a record that contains concrete functions to manage counters.
data CounterService = CounterService
    { counterServiceIncrement :: Key -> Count -> IO Counter
    , counterServiceQuery :: Key -> IO Counter
    }
