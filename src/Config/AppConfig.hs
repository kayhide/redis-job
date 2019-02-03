{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Config.AppConfig where

import           ClassyPrelude

import           Control.Lens        ((^.))
import           Control.Lens.TH

import           Config
import           Config.DbConfig
import qualified Plugin.Redis.Config as Redis


data AppConfig = AppConfig
  { _setting :: AppSetting
  , _running :: AppRunning
  }
  deriving (Eq, Show)

data AppSetting = AppSetting
  { _redis :: Redis.RedisSetting
  , _db    :: DbSetting
  }
  deriving (Eq, Show)

data AppRunning = AppRunning
  { _redis :: Redis.RedisRunning
  , _db    :: DbRunning
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

  activate setting' =
    AppRunning
    <$> activate (setting' ^. redis)
    <*> activate (setting' ^. db)


instance Redis.HasConfig AppConfig where
  setting = setting . redis
  running = running . redis
