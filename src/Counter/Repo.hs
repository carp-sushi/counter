module Counter.Repo where

import Counter.Domain (Count, Counter, Key)

-- | Counter repo is a mockable record that contains functions to manage counters.
data CounterRepo = CounterRepo
    { counterRepoIncrement :: Key -> Count -> IO Counter
    , counterRepoQuery :: Key -> IO Counter
    }
