{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Config.AppConfig where

import           ClassyPrelude

import           Control.Lens       ((^.))
import           Control.Lens.TH

import           Config
import           Config.DbConfig
import           Config.RedisConfig


data AppConfig = AppConfig
  { _setting :: AppSetting
  , _running :: AppRunning
  }
  deriving (Eq, Show)

data AppSetting = AppSetting
  { _redis :: RedisSetting
  , _db    :: DbSetting
  }
  deriving (Eq, Show)

data AppRunning = AppRunning
  { _redis :: RedisRunning
  , _db    :: DbRunning
  }
  deriving (Eq, Show)

$(makeFieldsNoPrefix ''AppConfig)
$(makeFieldsNoPrefix ''AppSetting)
$(makeFieldsNoPrefix ''AppRunning)

instance Configurable AppConfig where
  type Setting AppConfig = AppSetting
  type Running AppConfig = AppRunning

  build =
    AppSetting
    <$> build
    <*> build

  boot setting' =
    AppRunning
    <$> boot (setting' ^. redis)
    <*> boot (setting' ^. db)
