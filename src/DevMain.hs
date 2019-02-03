{-# LANGUAGE BlockArguments #-}
module DevMain where

import           ClassyPrelude

import           Control.Lens         ((^.))
import           Control.Monad.Reader
import qualified Data.Aeson           as Aeson
import           Database.Persist
import           Database.Persist.Sql
import           Database.Redis
import           System.Environment

import           Config
import           Config.AppConfig
import           Config.DbConfig
import           Config.RedisConfig
import           Model.Entities
import           Model.Predictor
import           TrainJob

run :: IO ()
run = do
  setting' :: AppSetting <- build
  running' <- boot setting'
  let config = AppConfig setting' running'
  print config

  runReaderT listJobs config
  runReaderT listPredictors config



listJobs :: (MonadReader AppConfig m, MonadIO m) => m ()
listJobs = do
  conn' <- asks (^. running . redis . conn)
  liftIO $ do
    jobs :: [Either String (JobWrapper TrainJob)] <- runRedis conn' $ do
      xs <- lrange "sidekiq_rb_development:queue:default" 0 (negate 1)
      case xs of
        Left _ -> fail "Failed to read queue"
        Right xs' -> do
          traverse_ print xs'
          pure $ fmap (Aeson.eitherDecode . fromStrict) xs'

    print jobs



listPredictors :: (MonadReader AppConfig m, MonadIO m, MonadUnliftIO m) => m ()
listPredictors = do
  pool' <- asks (^. running . db . pool)
  predictors <-
    flip runSqlPool pool' $ do
    predictors :: [Entity Predictor] <- selectList [] []
    pure predictors

  traverse_ print predictors
