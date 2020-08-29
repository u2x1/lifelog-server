{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Model where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Int                (Int64)
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateEvent"] [persistLowerCase|
Event
    createTime    Int64 Primary
    startTime     Int64
    endTime       Int64
    name          String
    nameSec       String Maybe
    type          Int
    modifyTime    Int64
    userName      String
    CreateAndUser createTime userName
|]
$(deriveJSON defaultOptions ''Event)

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User
    name        String Primary Unique
    password    String
    Username    name
|]
$(deriveJSON defaultOptions ''User)

share [mkPersist sqlSettings, mkMigrate "migrateUserSession"] [persistLowerCase|
UserSession
    key         String Primary Unique
    userName    String
    activeTime  Int64
    SessionKey  key
|]


