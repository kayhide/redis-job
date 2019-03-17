module Main where

import ClassyPrelude

import qualified Data.Aeson as Aeson

import Plugin.Sidekiq (JobWrapper)
import qualified Plugin.Sidekiq as Sidekiq

import App.Job.TrainJob
import qualified Config


main :: IO ()
main = do
  config <- Config.activate'
  runReaderT app config


type AppM a = ReaderT Config.Config IO a

app :: AppM ()
app =
  Sidekiq.watch $ \x -> do
    let x' = fromStrict x
    Sidekiq.SomeJob <$> (Aeson.decode x' :: Maybe (JobWrapper TrainJob))
