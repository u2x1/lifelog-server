{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Data.Api where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.PayloadType
import           Data.RetType
import           Database.Persist.Sqlite
import           Ops.Event
import           Ops.User
import           Servant
import           Util


app :: ConnectionPool -> Application
app pool = serve api $ hoistServer api (`runReaderT` pool) server

type API = UserAPI :<|> EventAPI

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = userServer :<|> eventServer




type UserAPI = "user" :>
  (    "login"    :> ReqBody '[JSON] UserPayload :> PostCreated '[JSON] RetJSON
  :<|> "register" :> ReqBody '[JSON] UserPayload :> PostCreated '[JSON] RetJSON
  :<|> "exist"    :> Capture "username" String :> Get '[JSON] Bool)

userServer :: ServerT UserAPI AppM
userServer = login :<|> register :<|> userExist

type EventAPI = "event" :>
  (    "upload" :> ReqBody '[JSON] Object :> Post '[JSON] RetJSON
  :<|> "fetch"  :> ReqBody '[JSON] Object :> Post '[JSON] RetJSON)

eventServer :: ServerT EventAPI AppM
eventServer = upload :<|> fetch
