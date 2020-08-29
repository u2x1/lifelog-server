{-# LANGUAGE TemplateHaskell #-}
module Data.PayloadType where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int      (Int64)
import           Data.Model


data EventPayload = EventPayload {
    event_id         :: Int64
  , event_startTime  :: Int64
  , event_endTime    :: Int64
  , event_modifyTime :: Int64
  , event_name       :: String
  , event_nameSec    :: Maybe String
  , event_type       :: Int
}
$(deriveFromJSON defaultOptions {fieldLabelModifier = drop 6} 'EventPayload)
$(deriveToJSON defaultOptions {fieldLabelModifier = drop 6} 'EventPayload)

payload2Event :: String -> EventPayload -> Event
payload2Event user payload = Event
  (event_id         payload)
  (event_startTime  payload)
  (event_endTime    payload)
  (event_name       payload)
  (event_nameSec    payload)
  (event_type  payload)
  (event_modifyTime payload)
  user

event2Event :: Event -> EventPayload
event2Event event = EventPayload
  (eventCreateTime event)
  (eventStartTime event)
  (eventEndTime event)
  (eventModifyTime event)
  (eventName event)
  (eventNameSec event)
  (eventType event)

data UserPayload = UserPayload {
    user_username :: String
  , user_password :: String
}
$(deriveFromJSON defaultOptions {fieldLabelModifier = drop 5} 'UserPayload)

payloadUser2User :: UserPayload -> User
payloadUser2User UserPayload{user_username = n, user_password = p}= User n p
