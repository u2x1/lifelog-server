
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.Event where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Model
import           Data.PayloadType
import           Data.RetType
import           Database.Persist
import           Util

guardJsonObj :: Either String (String, b) -> ((Entity UserSession, String, b) -> AppM RetJSON) -> AppM RetJSON
guardJsonObj obj action =
  case obj of
    Left err -> return $ RetJSON 202 ("json parse error: " <> err) Nothing
    Right (sessionKey, payload) -> do
         session' <- runDB (getBy (SessionKey sessionKey))
         case session' of
           Nothing -> return $ RetJSON 112 "user session not exists" Nothing
           Just session -> action (session, userSessionUserName (entityVal session), payload)


upload :: Object -> AppM RetJSON
upload jsonObj = do
  let obj = parseEither (\o -> do
                          session <- o .: "userSession"
                          events' <- o .: "events"
                          pure (session, events')) jsonObj
  guardJsonObj obj $ \(_, username, payload) -> do
   let events = fmap (payload2Event username) payload
   runDB (putMany events)
   return $ RetJSON 110 "upload successfully" Nothing

fetch :: Object -> AppM RetJSON
fetch jsonObj = do
  let obj = parseEither (\o -> do
                           session <- o .: "userSession"
                           time <- o .: "lastSyncTime"
                           pure (session, time)) jsonObj

  guardJsonObj obj $ \(session, username, payload) -> do
    events <- (entityVal <$>) <$> runDB (selectList [EventModifyTime >. payload, EventUserName ==. username] [])
    let payloads = map event2Event events
    time <- liftIO currentTimeStamp
    runDB (update (entityKey session) [UserSessionActiveTime =. time] )
    return $ RetJSON 110 "fetch successfully" (Just $ ModifiedEvents payloads)
