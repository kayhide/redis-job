{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module App.Config where

import           ClassyPrelude

import           Control.Lens          ((^.))
import           Control.Lens.TH

import           Configurable
import qualified Plugin.Db.Config      as Db
import qualified Plugin.Redis.Config   as Redis
import qualified Plugin.Sidekiq.Config as Sidekiq


data AppConfig = AppConfig
  { _setting :: AppSetting
  , _running :: AppRunning
  }
  deriving (Show)

data AppSetting = AppSetting
  { _redis   :: Redis.RedisSetting
  , _db      :: Db.DbSetting
  , _sidekiq :: Sidekiq.SidekiqSetting
  }
  deriving (Eq, Show)

data AppRunning = AppRunning
  { _redis   :: Redis.RedisRunning
  , _db      :: Db.DbRunning
  , _sidekiq :: Sidekiq.SidekiqRunning
  }
  deriving (Show)

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
    <*> ready

  start setting' =
    AppRunning
    <$> start (setting' ^. redis)
    <*> start (setting' ^. db)
    <*> start (setting' ^. sidekiq)


instance Db.HasConfig AppConfig where
  setting = setting . db
  running = running . db

instance Redis.HasConfig AppConfig where
  setting = setting . redis
  running = running . redis

instance Sidekiq.HasConfig AppConfig where
  setting = setting . sidekiq
  running = running . sidekiq
