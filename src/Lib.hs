module Lib (devMain) where

import App (app)
import qualified Database as DB
import Env (Env (..))
import Logger (stdoutLogging)

import Network.Wai.Handler.Warp (run)
import Say (say)

-- | Start the development counter server.
devMain :: IO ()
devMain = do
    conn <- DB.defaultConnection
    let env = Env (DB.newCounterService conn) stdoutLogging
    say "Running counter-server on port 9000"
    run 9000 (app env)
