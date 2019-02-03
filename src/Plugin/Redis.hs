module Plugin.Redis where

import           ClassyPrelude

import           Control.Lens        ((^.))
import qualified Database.Redis      as Redis

import           Plugin.Redis.Config


run
  :: (HasConfig env, MonadReader env m, MonadIO m)
  => Redis.Redis a
  -> m a
run redis = do
  conn' <- asks (^. running . conn)
  liftIO $ Redis.runRedis conn' redis
