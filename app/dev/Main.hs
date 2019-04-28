module Main where

import ClassyPrelude

import qualified App.Api as Api
import App.Api.Config (ApiConfig)
import Configurable (activate)
import qualified Plugin.Db as Db
import qualified Plugin.Logger as Logger
import qualified Plugin.Redis as Redis
import qualified Plugin.Sidekiq as Sidekiq

import qualified Conduct

type AppDeps =
  '[ Logger.Config
   , Db.Config
   , Redis.Config
   , Sidekiq.Config
   , ApiConfig
   ]

main :: IO ()
main = do
  Conduct.main

  config <- activate @AppDeps
  runReaderT (Api.start config) config
