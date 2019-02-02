{-# LANGUAGE TemplateHaskell #-}
module Config.RedisConfig where

import           ClassyPrelude

import           Control.Lens       ((^.))
import           Control.Lens.TH    (makeFieldsNoPrefix)
import           Database.Redis     (ConnectInfo, Connection, checkedConnect,
                                     parseConnectInfo)
import           System.Environment (lookupEnv)

import           Config


data RedisConfig

data RedisSetting = RedisSetting
  { _host :: Text
  , _port :: Text
  }
  deriving (Eq, Show)


data RedisRunning = RedisRunning
  { _info :: ConnectInfo
  , _conn :: Connection
  }

instance Eq RedisRunning where
  (==) _ _ = False

instance Show RedisRunning where
  show (RedisRunning info' _) =
    "RedisRunning "
    <> "{_info = " <> show info'
    <> ", _conn = Connection {...}"
    <> "}"

$(makeFieldsNoPrefix ''RedisSetting)
$(makeFieldsNoPrefix ''RedisRunning)


instance Configurable RedisConfig where
  type Setting RedisConfig = RedisSetting
  type Running RedisConfig = RedisRunning

  build =
    RedisSetting
    <$> (pack . fromMaybe "localhost" <$> lookupEnv "REDIS_HOST")
    <*> (pack . fromMaybe "6379" <$> lookupEnv "REDIS_PORT")

  boot setting =
    case parseConnectInfo (unpack url') of
      Left err -> fail $ pack err
      Right info' ->
        RedisRunning info' <$> checkedConnect info'
    where
      url' = "redis://" <> setting ^. host <> ":" <> setting ^. port
