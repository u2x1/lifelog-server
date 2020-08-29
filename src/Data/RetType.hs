{-# LANGUAGE TemplateHaskell #-}
module Data.RetType where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.PayloadType

data RetVal = UserSessionCode String
            | ModifiedEvents  [EventPayload]
$(deriveJSON defaultOptions ''RetVal)

data RetJSON = RetJSON {
    code   :: Int
  , msg    :: String
  , values :: Maybe RetVal
}
$(deriveJSON defaultOptions ''RetJSON)
