module App.Config where

import           ClassyPrelude

import           Data.Extensible
import           Data.Extensible.Plain
import           Data.Proxy

import           Configurable
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

  ready _ = do
    db' <- ready (Proxy @Db.DbConfig)
    redis' <- ready (Proxy @Redis.RedisConfig)
    sidekiq' <- ready (Proxy @Sidekiq.SidekiqConfig)
    pure $ db'
      <% redis'
      <% sidekiq'
      <% nil

  start _ setting' _ = do
    let conf' = (setting', nil)
    db' <- start (Proxy @Db.DbConfig) (pluck setting') conf'

    let conf'' = second (db' <%) conf'
    redis' <- start (Proxy @Redis.RedisConfig) (pluck setting') conf''

    let conf''' = second (redis' <%) conf''
    sidekiq' <- start (Proxy @Sidekiq.SidekiqConfig) (pluck setting') conf'''

    pure . snd $ second (shrink . (sidekiq' <%)) conf'''
