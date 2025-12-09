module Counter (
    devServer,
    devEnv,
    queryCounter,
    incrementCounter,
    decrementCounter,
) where

import Counter.App (app)
import qualified Counter.Database as DB
import Counter.Domain (Counter, Key)
import Counter.Env (Env (..), runAppT)
import Counter.Handler (deleteCountersH, getCountersH, postCountersH)
import Counter.Logger (stdoutLogging)

import Control.Monad.Logger (LogLevel (LevelDebug))
import Network.Wai.Handler.Warp (run)
import Say (say)

-- | Start the development counter server.
devServer :: IO ()
devServer = do
    env <- devEnv
    say "Counter DEV server running on port 3000"
    run 3000 (app env)

-- | Create a development environment.
devEnv :: IO Env
devEnv = do
    conn <- DB.defaultConnection
    pure $ Env (DB.newCounterRepo conn) stdoutLogging LevelDebug

-- | Get the counter for a given key.
queryCounter :: Key -> Env -> IO Counter
queryCounter key env =
    runAppT env $ getCountersH key

-- | Increment the counter for a given key.
incrementCounter :: Key -> Env -> IO Counter
incrementCounter key env =
    runAppT env $ postCountersH key

-- | Decrement the counter for a given key.
decrementCounter :: Key -> Env -> IO Counter
decrementCounter key env =
    runAppT env $ deleteCountersH key
