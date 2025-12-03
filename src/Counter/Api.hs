{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Counter.Api (Api, api) where

import Counter.Domain (Counter, Key)

import Data.Proxy
import Data.Text (Text)
import Servant.API

-- | Root URI type for the API.
type Root uri = "counters" :> "api" :> "v1" :> uri

-- | The top-level API type.
type Api =
    "status" :> Get '[PlainText] Text
        :<|> Root (Capture "key" Key :> Post '[JSON] Counter)
        :<|> Root (Capture "key" Key :> Delete '[JSON] Counter)
        :<|> Root (Capture "key" Key :> Get '[JSON] Counter)

{- API boilerplate. This allows us to pass around type information
   without a concrete value of the API type available. -}
api :: Proxy Api
api = Proxy
