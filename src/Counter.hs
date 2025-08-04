module Counter (devMain) where

import Counter.App (app)
import qualified Counter.Database as DB
import Counter.Env (Env (..))
import Counter.Logger (stdoutLogging)

import Network.Wai.Handler.Warp (run)
import Say (say)

-- | Start the development counter server.
devMain :: IO ()
devMain = do
    conn <- DB.defaultConnection
    let env = Env (DB.newCounterService conn) stdoutLogging
    say "Running counter-server on port 9000"
    run 9000 (app env)
