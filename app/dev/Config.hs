module Config where

import           ClassyPrelude

import           Data.Extensible
import           Data.Extensible.Plain

import           Configurable
import           Plugin.Db.Config      (DbConfig)
import           Plugin.Logger.Config  (LoggerConfig)
import           Plugin.Redis.Config   (RedisConfig)
import           Plugin.Sidekiq.Config (SidekiqConfig)

import           App.Api.Config        (ApiConfig)


type Config = AllOf (ToConfigs
  '[ LoggerConfig
   , DbConfig
   , RedisConfig
   , SidekiqConfig
   , ApiConfig
   ]
  )

activate' :: IO Config
activate' =
  pure nil
  >>= activate @LoggerConfig
  >>= activate @DbConfig
  >>= activate @RedisConfig
  >>= activate @SidekiqConfig
  >>= activate @ApiConfig
  >>= pure . shrink
