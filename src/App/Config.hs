{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module App.Config where

import           ClassyPrelude

import           Control.Lens          ((^.), _1, _2)
import           Control.Lens.TH

import           Configurable
import qualified Plugin.Db.Config      as Db
import qualified Plugin.Redis.Config   as Redis
import qualified Plugin.Sidekiq.Config as Sidekiq


type AppConfig = (AppSetting, AppRunning)

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


instance HasConfig AppConfig Db.DbConfig where
  setting = _1 . db
  running = _2 . db

instance HasConfig AppConfig Redis.RedisConfig where
  setting = _1 . redis
  running = _2 . redis

instance HasConfig AppConfig Sidekiq.SidekiqConfig where
  setting = _1 . sidekiq
  running = _2 . sidekiq
