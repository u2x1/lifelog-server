module Ops.User where

import           Control.Monad.Reader
import           Data.Model
import           Data.PayloadType
import           Data.RetType
import           Database.Persist
import           Util

login :: UserPayload -> AppM RetJSON
login user = do
  userInDb <- runDB (getBy (Username (user_username user)))
  case userInDb of
    Nothing -> return $ RetJSON 102 "username not exists" Nothing
    Just realUser' -> do
      let realUser = entityVal realUser'
      if user_password user /= userPassword realUser
        then return $ RetJSON 103 "incorrect password" Nothing
        else do
          key <- liftIO randomString
          curnTime <- liftIO currentTimeStamp
          _ <- runDB (insert (UserSession key (user_username user) curnTime))
          liftIO $ return $ RetJSON 100 "login successfully" (Just $ UserSessionCode key)

register :: UserPayload -> AppM RetJSON
register user = do
  userInDb <- runDB (getBy (Username (user_username user)))
  case userInDb of
    Just _ -> return $ RetJSON 101 "username already exists" Nothing
    _      -> do
      _ <- runDB (insert $ payloadUser2User user)
      liftIO $ return $ RetJSON 100 "register successfully" Nothing


userExist :: String -> AppM Bool
userExist username = do
  userInDb <- runDB (getBy (Username username))
  case userInDb of
    Just _ -> return True
    _      -> return False
