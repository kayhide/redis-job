module Main where

import           ClassyPrelude

import qualified Data.Aeson           as Aeson

import           App.Config           (AppConfig)
import           App.Job.TrainJob
import           Configurable         (activate)
import           Plugin.Sidekiq       (JobWrapper)
import qualified Plugin.Sidekiq       as Sidekiq


main :: IO ()
main = do
  config :: AppConfig <- activate ()
  runReaderT app config


type AppM a = ReaderT AppConfig IO a

app :: AppM ()
app =
  Sidekiq.watch $ \queue' x -> do
    putStrLn $ "Fetched from queue: " <> queue'
    case (Aeson.eitherDecode . fromStrict) x of
      Left msg                           -> putStrLn . pack $ msg
      Right (job :: JobWrapper TrainJob) -> Sidekiq.performNow job
