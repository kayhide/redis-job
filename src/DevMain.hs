module DevMain where

import           ClassyPrelude

import qualified Data.Aeson           as Aeson
import           Database.Persist
import           Database.Persist.Sql
import qualified Database.Redis       as Redis

import           App.Config           (AppConfig (..))
import           Configurable         (activate)
import           Model.Entities
import           Model.Predictor
import qualified Plugin.Db            as Db
import qualified Plugin.Redis         as Redis
import           TrainJob

run :: IO ()
run = do
  config <- uncurry AppConfig <$> activate
  print config
  runReaderT app config


type AppM a = ReaderT AppConfig IO a

queue :: Text
queue ="sidekiq_rb_development:queue:default"

app :: AppM ()
app = do
  listJobs
  listPredictors
  forever $ do
    Redis.run $ do
      Redis.brpop [encodeUtf8 queue] 1 >>= \case
        Left _ -> fail "Failed to read queue"
        Right x' -> case x' of
          Nothing -> print x'
          Just (queue', x'') -> do
            putStrLn $ "Fetched from queue: " <> decodeUtf8 queue'
            case (Aeson.eitherDecode . fromStrict) x'' of
              Left msg                           -> putStrLn . pack $ msg
              Right (job :: JobWrapper TrainJob) -> print job


listJobs :: AppM ()
listJobs = do
  jobs :: [Either String (JobWrapper TrainJob)] <- Redis.run $ do
    xs <- Redis.lrange (encodeUtf8 queue) 0 (negate 1)
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
