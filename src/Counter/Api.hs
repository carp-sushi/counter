{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Counter.Api (Api, api) where

import Counter.Domain

import Data.Proxy
import Data.Text (Text)
import Servant.API

-- | Root URI type for the API.
type Root uri = "counters" :> "api" :> "v1" :> uri

-- | The top-level API type.
type Api =
    "status" :> Get '[PlainText] Text
        :<|> Root (Capture "key" Key :> PostNoContent)
        :<|> Root (Capture "key" Key :> DeleteNoContent)
        :<|> Root (Capture "key" Key :> Get '[JSON] Counter)

{- API boilerplate. This allows us to pass around type information
   without needing a concrete value available of the API type. -}
api :: Proxy Api
api = Proxy
