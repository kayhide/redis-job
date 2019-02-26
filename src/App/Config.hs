module App.Config where

import           ClassyPrelude

import           Data.Extensible
import           Data.Extensible.Plain

import           Configurable
import           Plugin.Db.Config      (DbConfig)
import           Plugin.Logger.Config  (LoggerConfig)
import           Plugin.Redis.Config   (RedisConfig)
import           Plugin.Sidekiq.Config (SidekiqConfig)


type AppConfig = AllOf (ToConfigs
  '[ LoggerConfig
   , DbConfig
   , RedisConfig
   , SidekiqConfig
   ]
  )

activate' :: IO AppConfig
activate' =
  pure nil
  >>= activate @LoggerConfig
  >>= activate @DbConfig
  >>= activate @RedisConfig
  >>= activate @SidekiqConfig
  >>= pure . shrink
