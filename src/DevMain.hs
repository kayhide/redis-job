{-# LANGUAGE BlockArguments #-}
module DevMain where

import           ClassyPrelude

import           Control.Lens         ((^.))
import           Control.Monad.Reader
import qualified Data.Aeson           as Aeson
import           Database.Redis
import           System.Environment

import           Config
import           Config.AppConfig
import           Config.RedisConfig
import           TrainJob

run :: IO ()
run = do
  setting' :: AppSetting <- build
  running' <- boot setting'
  let config = AppConfig setting' running'
  print config

  runReaderT listJobs config



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

