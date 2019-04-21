module Plugin.Redis where

import ClassyPrelude

import Configurable (ToConfig, running)
import Control.Lens (view)
import Data.Extensible ((:*), Member)
import Database.Redis as Redis
import Plugin.Redis.Config


type Config = RedisConfig

run
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => Redis.Redis a
  -> m a
run redis = do
  conn' <- view $ running @_ @Config . conn
  liftIO $ Redis.runRedis conn' redis

brpop
  :: (RedisCtx m f)
  => [ByteString]
  -> Integer
  -> m (f (Maybe (ByteString, ByteString)))
brpop = Redis.brpop
