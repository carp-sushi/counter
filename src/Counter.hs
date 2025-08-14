module Counter (
    devServer,
    getCounter,
    incrementCounter,
    decrementCounter,
) where

import Counter.App (app)
import qualified Counter.Database as DB
import Counter.Domain (Counter, Key)
import Counter.Env (Env (..), runAppT)
import Counter.Handler (deleteCountersH, getCountersH, postCountersH)
import Counter.Logger (stdoutLogging)

import Network.Wai.Handler.Warp (run)
import Say (say)

-- | Start the development counter server.
devServer :: IO ()
devServer = do
    env <- devEnv
    say "Running counter-server on port 9000"
    run 9000 (app env)

-- | Create a development environment.
devEnv :: IO Env
devEnv = do
    conn <- DB.defaultConnection
    return $ Env (DB.newCounterService conn) stdoutLogging

-- | Get the counter for a given key.
getCounter :: Env -> Key -> IO Counter
getCounter env key =
    runAppT env $ getCountersH key

-- | Increment the counter for a given key.
incrementCounter :: Env -> Key -> IO Counter
incrementCounter env key =
    runAppT env $ postCountersH key

-- | Decrement the counter for a given key.
decrementCounter :: Env -> Key -> IO Counter
decrementCounter env key =
    runAppT env $ deleteCountersH key
