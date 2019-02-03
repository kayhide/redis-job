{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module App.Config where

import           ClassyPrelude

import           Control.Lens        ((^.))
import           Control.Lens.TH

import           Configurable
import qualified Plugin.Db.Config    as Db
import qualified Plugin.Redis.Config as Redis


data AppConfig = AppConfig
  { _setting :: AppSetting
  , _running :: AppRunning
  }
  deriving (Eq, Show)

data AppSetting = AppSetting
  { _redis :: Redis.RedisSetting
  , _db    :: Db.DbSetting
  }
  deriving (Eq, Show)

data AppRunning = AppRunning
  { _redis :: Redis.RedisRunning
  , _db    :: Db.DbRunning
  }
  deriving (Eq, Show)

$(makeFieldsNoPrefix ''AppConfig)
$(makeFieldsNoPrefix ''AppSetting)
$(makeFieldsNoPrefix ''AppRunning)

instance Configurable AppConfig where
  type Setting AppConfig = AppSetting
  type Running AppConfig = AppRunning

  ready =
    AppSetting
    <$> ready
    <*> ready

  start setting' =
    AppRunning
    <$> start (setting' ^. redis)
    <*> start (setting' ^. db)


instance Db.HasConfig AppConfig where
  setting = setting . db
  running = running . db

instance Redis.HasConfig AppConfig where
  setting = setting . redis
  running = running . redis
