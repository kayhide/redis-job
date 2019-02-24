module Main where

import           ClassyPrelude

import qualified Data.Aeson           as Aeson

import           App.Config           (AppConfig, activate')
import           App.Job.TrainJob
import           Plugin.Sidekiq       (JobWrapper)
import qualified Plugin.Sidekiq       as Sidekiq


main :: IO ()
main = do
  config :: AppConfig <- activate'
  runReaderT app config


type AppM a = ReaderT AppConfig IO a

app :: AppM ()
app =
  Sidekiq.watch $ \x -> do
    let x' = fromStrict x
    Sidekiq.SomeJob <$> (Aeson.decode x' :: Maybe (JobWrapper TrainJob))
