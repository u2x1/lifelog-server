{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Data.Api
import Data.Model
import System.Directory
import Control.Monad.IO.Class
import Database.Persist.Sqlite
import Control.Monad.Logger
import qualified Toml
import Toml ((.=), TomlCodec)

main :: IO ()
main = do
  cfg <- parseConfig "config.toml"
  
  runStderrLoggingT $ withSqlitePool "lifelog.db" 20 $ \pool -> do
    runSqlPool (runMigration migrateUser >>
                runMigration migrateEvent >>
                runMigration migrateUserSession) pool
    liftIO $ runTLS 
            (tlsSettings (cert_fullchain cfg) (cert_key cfg))
            (setPort (port cfg) defaultSettings)
            (app pool)


test :: IO ()
test =
  runStderrLoggingT $ withSqlitePool "lifelog.db" 20 $ \pool -> do
    runSqlPool (runMigration migrateUser >> runMigration migrateEvent >> runMigration migrateUserSession) pool
    liftIO $ run 23841 (app pool)
    



fallbackConfig :: String
fallbackConfig = "port = 23841\n\
                 \cert_key = \"server.key\"\n\
                 \cert_fullchain = \"server.pem\""

parseConfig :: FilePath -> IO Config
parseConfig path = do
  exist <- doesDirectoryExist path
  _ <- if exist
          then pure ()
          else do
            putStrLn "warning: configuration file was not found. falling back to default config"
            writeFile path fallbackConfig
  tomlRes <- Toml.decodeFileEither configCodec path
  case tomlRes of
    Left errs -> do
      error $ "error: parsing config failed: " ++ (show errs)
    Right config -> pure config

data Config = Config {
    port           :: Int
  , cert_key       :: String
  , cert_fullchain :: String
} deriving (Show)

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.int "port"                .= port
  <*> Toml.string "cert_key"         .= cert_key
  <*> Toml.string "cert_fullchain"   .= cert_fullchain
