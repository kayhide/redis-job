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
import           Model.Entities
import           Model.Predictor
import qualified Plugin.Redis         as Redis
import           TrainJob

run :: IO ()
run = do
  config <- uncurry AppConfig <$> start
  print config

  runReaderT listJobs config
  runReaderT listPredictors config

type AppM a = ReaderT AppConfig IO a


listJobs :: AppM ()
listJobs = do
  jobs :: [Either String (JobWrapper TrainJob)] <- Redis.run $ do
    xs <- lrange "sidekiq_rb_development:queue:default" 0 (negate 1)
    case xs of
      Left _ -> fail "Failed to read queue"
      Right xs' -> do
        traverse_ print xs'
        pure $ fmap (Aeson.eitherDecode . fromStrict) xs'

  print jobs



listPredictors :: AppM ()
listPredictors = do
  pool' <- asks (^. running . db . pool)
  predictors <-
    flip runSqlPool pool' $ do
    predictors :: [Entity Predictor] <- selectList [] []
    pure predictors

  traverse_ print predictors
