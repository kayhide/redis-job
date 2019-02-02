module DevMain where

import           ClassyPrelude

import qualified Data.Aeson         as Aeson
import           Database.Redis
import           System.Environment

import           TrainJob

run :: IO ()
run = do
  redisPort <- lookupEnv "REDIS_PORT"
  let redisUrl = "redis://localhost:" <> fromMaybe "6379" redisPort
  case parseConnectInfo redisUrl of
    Left err -> putStrLn $ pack err
    Right info -> do
      conn <- checkedConnect info
      listJobs conn

listJobs :: Connection -> IO ()
listJobs conn = do
  jobs :: [Either String (JobWrapper TrainJob)] <- runRedis conn $ do
    xs <- lrange "sidekiq_rb_development:queue:default" 0 (negate 1)
    case xs of
      Left _ -> fail "Failed to read queue"
      Right xs' -> do
        traverse_ print xs'
        pure $ fmap (Aeson.eitherDecode . fromStrict) xs'

  print jobs

