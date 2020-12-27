{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
module Lib
    ( startApp
    -- , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Text (Text)
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Debug.Trace (traceM, trace)

-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''User)

type Store = IORef (M.Map Text Value)

type API = "get" :> Capture "key" Text :> Get '[JSON] Value
        :<|> "put" :> Capture "key" Text :> ReqBody '[JSON] Value :> Post '[JSON] Text
-- type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = do
  store <- liftIO $ newIORef M.empty
  run 8080 $ serve api $ server store

-- app :: Store -> Application
-- app store = do
--   serve api $ server store

api :: Proxy API
api = Proxy

server :: Store -> Server API
server store = getKey store :<|> putKey store

getKey :: Store -> Text -> Handler Value
getKey store key = do
  s <- liftIO $ readIORef store
  traceM . show $ M.toList s
  return $ case M.lookup key s of
    Just t -> t
    Nothing -> ""

putKey :: Store -> Text -> Value -> Handler Text
putKey store key value = do
  s <- liftIO $ readIORef store
  traceM . show $ M.toList s
  liftIO $ atomicModifyIORef' store $ \s -> (M.insert key value s, key)
  s <- liftIO $ readIORef store
  traceM . show $ M.toList s
  return key

-- users :: [User]
-- users = [ User 1 "Isaac" "Newton"
--         , User 2 "Albert" "Einstein"
--         ]
