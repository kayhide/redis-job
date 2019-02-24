module DevMain where

import           ClassyPrelude

import qualified Data.Aeson           as Aeson
import           Database.Persist
import           Database.Persist.Sql
import qualified Database.Redis       as Redis

import           App.Config           (AppConfig, activate')
import           App.Job.TrainJob
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
  Sidekiq.watch $ \x -> do
    let x' = fromStrict x
    Sidekiq.SomeJob <$> (Aeson.decode x' :: Maybe (JobWrapper TrainJob))


listJobs :: AppM ()
listJobs = do
  putStrLn "*** Listing Jobs"
  queues' <- Sidekiq.askQueues
  traverse_ print =<< concat <$> traverse jobs' queues'
  putStrLn ""

  where
    jobs' :: Text -> AppM [Either String (JobWrapper TrainJob)]
    jobs' queue = Redis.run $ do
      xs <- Redis.lrange (encodeUtf8 queue) 0 (-1)
      case xs of
        Left err  -> fail $ "Failed to read queue: " <> show err
        Right xs' -> do
          pure $ Aeson.eitherDecode . fromStrict <$> xs'




listPredictors :: AppM ()
listPredictors = do
  putStrLn "*** Listing Predictors"
  predictors :: [Entity Predictor] <- Db.run $ selectList [] []
  traverse_ print predictors
  putStrLn ""
