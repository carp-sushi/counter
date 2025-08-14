module Counter.App (app) where

import Counter.Api (Api, api)
import Counter.Env (AppT (..), Env, runAppT)
import Counter.Handler (deleteCountersH, getCountersH, getStatusH, postCountersH)

import Control.Monad.Reader (MonadIO)
import Servant

-- | Create a Servant application with a given environment.
app :: Env -> Application
app env =
    let server = hoistServer api (runAppT env) handlers
     in serve api server

-- | Combine request handlers for the API.
handlers :: (MonadIO m) => ServerT Api (AppT m)
handlers =
    getStatusH
        :<|> postCountersH
        :<|> deleteCountersH
        :<|> getCountersH
