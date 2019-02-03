{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Config.DbConfig where

import           ClassyPrelude

import           Control.Lens.TH             (makeFieldsNoPrefix)
import           Control.Monad.Logger
import           Data.Pool                   (Pool)
import           Database.Persist.Postgresql
import           System.Environment          (lookupEnv)

import           Config


data DbConfig

data DbSetting = DbSetting
  { _host :: Text
  , _port :: Text
  , _database :: Text
  , _user :: Text
  , _pool :: Int
  }
  deriving (Eq, Show)

data DbRunning = DbRunning
  { _pool :: Pool SqlBackend
  }

instance Eq DbRunning where
  (==) _ _ = False

instance Show DbRunning where
  show (DbRunning _) =
    "DbRunning "
    <> "{_pool = = Pool {...}"
    <> "}"

$(makeFieldsNoPrefix ''DbRunning)
$(makeFieldsNoPrefix ''DbConfig)



instance Configurable DbConfig where
  type Setting DbConfig = DbSetting
  type Running DbConfig = DbRunning

  build =
    DbSetting
    <$> (pack . fromMaybe "localhost" <$> lookupEnv "DB_HOST")
    <*> (pack . fromMaybe "5432" <$> lookupEnv "DB_PORT")
    <*> (pack . fromMaybe "database" <$> lookupEnv "DB_DATABASE")
    <*> (pack . fromMaybe "postgres" <$> lookupEnv "DB_USER")
    <*> (fromMaybe 5 . (readMay =<<) <$> lookupEnv "DB_POOL")

  boot (DbSetting host' port' database' user' pool') = do
    let connstr =
          "host=" <> host'
          <> " dbname=" <> database'
          <> " user=" <> user'
          <> " port=" <> port'
    pool'' <- runStdoutLoggingT $ createPostgresqlPool (encodeUtf8 connstr) pool'
    pure $ DbRunning pool''

