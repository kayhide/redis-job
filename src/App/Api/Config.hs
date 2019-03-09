{-# LANGUAGE TemplateHaskell #-}
module App.Api.Config where

import           ClassyPrelude

import           Control.Lens         ((^.))
import           Control.Lens.TH      (makeFieldsNoPrefix)

import           Configurable         (Configurable (..), HasConfig (..),
                                       fetchSetting)
import qualified Plugin.Logger        as Logger
import           Plugin.Logger.Config (LoggerConfig)


data ApiConfig

data ApiSetting = ApiSetting
  { _port        :: Int
  , _proxiedHost :: Text
  , _proxiedPort :: Int
  }
  deriving (Eq, Show)


data ApiRunning = ApiRunning

instance Show ApiRunning where
  show _ =
    "ApiRunning"

$(makeFieldsNoPrefix ''ApiSetting)
$(makeFieldsNoPrefix ''ApiRunning)


instance Configurable ApiConfig where
  type Setting ApiConfig = ApiSetting
  type Running ApiConfig = ApiRunning
  type Deps ApiConfig = '[LoggerConfig]

  ready =
    ApiSetting
    <$> fetchSetting "WEB_PORT" 8080
    <*> fetchSetting "WEB_PROXIED_HOST" "127.0.0.1"
    <*> fetchSetting "WEB_PROXIED_PORT" 5100

  start _ _ = pure ApiRunning
