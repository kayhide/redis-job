module Main where

import ClassyPrelude

import App.Job.TrainJob
import Configurable (ToConfig, activate)
import qualified Data.Aeson as Aeson
import Data.Extensible ((:*))
import qualified Plugin.Db as Db
import qualified Plugin.Logger as Logger
import qualified Plugin.Redis as Redis
import Plugin.Sidekiq (JobWrapper)
import qualified Plugin.Sidekiq as Sidekiq


type AppDeps =
  '[ Logger.Config
   , Db.Config
   , Redis.Config
   , Sidekiq.Config
   ]

type AppConfig = ToConfig :* AppDeps

main :: IO ()
main = do
  config <- activate @AppDeps
  runReaderT app config


type AppM a = ReaderT AppConfig IO a

app :: AppM ()
app =
  Sidekiq.watch $ \x -> do
    let x' = fromStrict x
    Sidekiq.SomeJob <$> (Aeson.decode x' :: Maybe (JobWrapper TrainJob))
