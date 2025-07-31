module App (app) where

import Api
import Env
import Handler

import Control.Monad.Reader (MonadIO, runReaderT)
import Servant

-- | Create the application from an environment.
app :: Env -> Application
app env =
    serve api $
        hoistServer api (transform env) handlers

-- | Transform handlers for servant.
transform :: Env -> AppT m a -> m a
transform env appt =
    runReaderT (unAppT appt) env

-- | Combine request handlers for the API.
handlers :: (MonadIO m) => ServerT Api (AppT m)
handlers =
    getStatusH
        :<|> postCountersH
        :<|> deleteCountersH
        :<|> getCountersH
