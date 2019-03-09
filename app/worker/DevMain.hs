module DevMain where

import           ClassyPrelude

import qualified Data.Aeson        as Aeson
import           Database.Persist
import qualified Database.Redis    as Redis

import qualified Plugin.Db         as Db
import qualified Plugin.Logger     as Logger
import qualified Plugin.Redis      as Redis
import           Plugin.Sidekiq    (JobWrapper)
import qualified Plugin.Sidekiq    as Sidekiq

import           Model.Entities
import           App.Job.TrainJob
import qualified Config


run :: IO ()
run = do
  config <- Config.activate'
  runReaderT app config


type AppM a = ReaderT Config.Config IO a

app :: AppM ()
app = do
  Logger.info . tshow =<< ask
  listJobs
  listPredictors
  Sidekiq.watch $ \x -> do
    let x' = fromStrict x
    Sidekiq.SomeJob <$> (Aeson.decode x' :: Maybe (JobWrapper TrainJob))


listJobs :: AppM ()
listJobs = do
  Logger.info "*** Listing Jobs"
  queues' <- Sidekiq.askQueues
  traverse_ print =<< concat <$> traverse jobs' queues'

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
  Logger.info "*** Listing Predictors"
  predictors :: [Entity Predictor] <- Db.run $ selectList [] []
  traverse_ (Logger.info . tshow) predictors
