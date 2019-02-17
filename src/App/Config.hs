{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module App.Config where

import           ClassyPrelude

import           Control.Lens          (_1, _2)

import           Configurable
import           Data.Extensible
import           Data.Extensible.Plain
import qualified Plugin.Db.Config      as Db
import qualified Plugin.Redis.Config   as Redis
import qualified Plugin.Sidekiq.Config as Sidekiq


type AppConfig = (AppSetting, AppRunning)

type AppSetting = AllOf
  '[ Db.DbSetting
   , Redis.RedisSetting
   , Sidekiq.SidekiqSetting
   ]

type AppRunning = AllOf
  '[ Db.DbRunning
   , Redis.RedisRunning
   , Sidekiq.SidekiqRunning
   ]

instance Configurable AppConfig where
  type Setting AppConfig = AppSetting
  type Running AppConfig = AppRunning
  type Deps AppConfig = '[]

  ready = do
    db' <- ready
    redis' <- ready
    sidekiq' <- ready
    pure $ db'
      <% redis'
      <% sidekiq'
      <% nil

  start setting' _ = do
    let conf' = (setting', nil)
    db' <- start (pluck setting' :: Db.DbSetting) conf'

    let conf'' = second (db' <%) conf'
    redis' <- start (pluck setting' :: Setting Redis.RedisConfig) conf''

    let conf''' = second (redis' <%) conf''
    sidekiq' <- start (pluck setting' :: Setting Sidekiq.SidekiqConfig) conf'''

    pure . snd $ second (shrink . (sidekiq' <%)) conf'''


instance ( Configurable a
         , Member settings (Setting a)
         , Member runnings (Running a)
         ) => HasConfig (AllOf settings, AllOf runnings) a where
  setting = _1 . item (Proxy @(Setting a))
  running = _2 . item (Proxy @(Running a))
