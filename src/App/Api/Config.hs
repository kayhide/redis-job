{-# LANGUAGE TemplateHaskell #-}
module App.Api.Config where

import ClassyPrelude hiding (Handler)

import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)

import Configurable (Configurable (..), fetchSetting)
import qualified Plugin.Logger as Logger


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
  type Deps ApiConfig = '[Logger.Config]

  ready =
    ApiSetting
    <$> fetchSetting "WEB_PORT" 8080
    <*> fetchSetting "WEB_PROXIED_HOST" "127.0.0.1"
    <*> fetchSetting "WEB_PROXIED_PORT" 5100

  start _ _ = pure ApiRunning


newtype AppM env a = AppM { unAppM :: ReaderT env IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadCatch
    , MonadThrow
    , MonadMask
    , MonadReader env
    )
