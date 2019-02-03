module DevMain where

import           ClassyPrelude

import qualified Data.Aeson           as Aeson
import           Database.Persist
import           Database.Persist.Sql
import           Database.Redis

import           App.Config           (AppConfig (..))
import           Configurable         (start)
import           Model.Entities
import           Model.Predictor
import qualified Plugin.Db            as Db
import qualified Plugin.Redis         as Redis
import           TrainJob

run :: IO ()
run = do
  config <- uncurry AppConfig <$> start
  print config
  runReaderT app config


type AppM a = ReaderT AppConfig IO a

app :: AppM ()
app = do
  listJobs
  listPredictors


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
  predictors :: [Entity Predictor] <- Db.run $ selectList [] []
  traverse_ print predictors
