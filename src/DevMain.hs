module DevMain where

import           ClassyPrelude

import qualified Data.Aeson           as Aeson
import           Database.Persist
import           Database.Persist.Sql
import qualified Database.Redis       as Redis

import           App.Config           (AppConfig (..), activate')
import           App.Job.TrainJob
import           Configurable         (activate)
import           Model.Entities
import           Model.Predictor
import qualified Plugin.Db            as Db
import qualified Plugin.Redis         as Redis
import           Plugin.Sidekiq       (JobWrapper)
import qualified Plugin.Sidekiq       as Sidekiq

run :: IO ()
run = do
  config :: AppConfig <- activate'
  print config
  runReaderT app config


type AppM a = ReaderT AppConfig IO a

app :: AppM ()
app = do
  listJobs
  listPredictors
  Sidekiq.watch $ \queue' x -> do
    putStrLn $ "Fetched from queue: " <> queue'
    case (Aeson.eitherDecode . fromStrict) x of
      Left msg                           -> putStrLn . pack $ msg
      Right (job :: JobWrapper TrainJob) -> Sidekiq.performNow job


listJobs :: AppM ()
listJobs = do
  queue' <- Sidekiq.queue
  jobs :: [Either String (JobWrapper TrainJob)] <- Redis.run $ do
    xs <- Redis.lrange (encodeUtf8 queue') 0 (-1)
    case xs of
      Left err  -> fail $ "Failed to read queue: " <> show err
      Right xs' -> do
        traverse_ print xs'
        pure $ fmap (Aeson.eitherDecode . fromStrict) xs'

  print jobs



listPredictors :: AppM ()
listPredictors = do
  predictors :: [Entity Predictor] <- Db.run $ selectList [] []
  traverse_ print predictors
