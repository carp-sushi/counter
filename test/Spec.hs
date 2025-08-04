import Counter.App (app)
import Counter.Env (Env (..))
import Counter.Logger (noLogging)
import Counter.Service (CounterService (..))
import qualified State as S

import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Types
import Servant hiding (Header)

import Test.Hspec
import Test.Hspec.Wai
import Test.Tasty
import Test.Tasty.Hspec

-- Setup test application.
setupApp :: (MonadIO m) => m Application
setupApp =
    liftIO $ do
        state <- S.newState
        let counterService = S.newCounterService state
        return $ app $ Env counterService noLogging

-- POST helper (no headers or body).
postUrl url =
    request methodPost url [] ""

-- Test for getting the server status.
spec_status :: Spec
spec_status =
    with setupApp $ do
        describe "GET /status" $ do
            it "should get the server status" $ do
                get "/status" `shouldRespondWith` 200

-- Test for incrementing counters.
spec_increment :: Spec
spec_increment =
    with setupApp $ do
        describe "POST /counters" $ do
            it "should create a counter for a key" $ do
                postUrl "/counters/api/v1/test" `shouldRespondWith` 204
            it "should fail with missing key" $ do
                postUrl "/counters/api/v1" `shouldRespondWith` 404

-- Test for decrementing counters.
spec_decrement :: Spec
spec_decrement =
    with setupApp $ do
        describe "DELETE /counters" $ do
            it "should decrement a counter for a key" $ do
                delete "/counters/api/v1/test" `shouldRespondWith` 204
            it "should fail with missing key" $ do
                delete "/counters/api/v1" `shouldRespondWith` 404

-- Test for getting counters.
spec_query :: Spec
spec_query =
    with setupApp $ do
        describe "GET /counters" $ do
            it "should get the counter for a key" $ do
                get "/counters/api/v1/test" `shouldRespondWith` 200
            it "should fail with missing key" $ do
                get "/counters/api/v1" `shouldRespondWith` 404

-- Collect all specs
allSpecs :: [Spec]
allSpecs =
    [ spec_status
    , spec_increment
    , spec_decrement
    , spec_query
    ]

-- Run tests
main :: IO ()
main = do
    specs <- concat <$> mapM testSpecs allSpecs
    defaultMain (testGroup "counter specs" specs)
