module Util where

import           Control.Monad           (replicateM)
import           Data.Int
import           Data.Time
import           Data.Time.Clock.POSIX
import           System.Random

import           Control.Monad.Reader    (ReaderT, ask, liftIO)
import           Database.Persist.Sqlite (ConnectionPool, SqlPersistT,
                                          runSqlPool)
import           Servant                 (Handler)

randomString :: IO String
randomString = randomString' 8

randomString' :: Int ->  IO String
randomString' = flip replicateM randomChar
  where randomChar = (pure . (charSet !!)) =<< randomRIO (0, length charSet - 1)
        charSet = ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z']

currentTimeStamp :: IO Int64
currentTimeStamp = utcTime2TimeStamp <$> getCurrentTime
  where utcTime2TimeStamp = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

type AppM = ReaderT ConnectionPool Handler

runDB :: SqlPersistT IO a -> AppM a
runDB query = do
  pool <- ask
  liftIO $ runSqlPool query pool
