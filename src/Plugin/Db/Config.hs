{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Plugin.Db.Config where

import ClassyPrelude

import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Monad.Logger
import Data.Pool (Pool, createPool)
import Database.Beam
import Database.Beam.Postgres

import Configurable (Configurable (..), fetchSetting)


data DbConfig

data DbSetting = DbSetting
  { _host     :: Text
  , _port     :: Text
  , _database :: Text
  , _user     :: Text
  , _pool     :: Int
  }
  deriving (Eq, Show)

data DbRunning = DbRunning
  { _pool :: Pool Connection
  }

instance Show DbRunning where
  show (DbRunning _) =
    "DbRunning "
    <> "{_pool = Pool {...}"
    <> "}"

$(makeFieldsNoPrefix ''DbRunning)
$(makeFieldsNoPrefix ''DbConfig)


instance Configurable DbConfig where
  type Setting DbConfig = DbSetting
  type Running DbConfig = DbRunning
  type Deps DbConfig = '[]

  ready =
    DbSetting
    <$> fetchSetting "DB_HOST" "localhost"
    <*> fetchSetting "DB_PORT" "5432"
    <*> fetchSetting "DB_DATABASE" "database"
    <*> fetchSetting "DB_USER" "postgres"
    <*> fetchSetting "DB_POOL" 5

  start (DbSetting host' port' database' user' pool') _ = do
    let connstr =
          "host=" <> host'
          <> " dbname=" <> database'
          <> " user=" <> user'
          <> " port=" <> port'
    pool'' <- createPool (connectPostgreSQL (encodeUtf8 connstr)) close 4 1 pool'
    pure $ DbRunning pool''
