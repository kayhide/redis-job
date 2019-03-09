{-# LANGUAGE DeriveAnyClass #-}
module App.Api
  ( start
  )
where

import           ClassyPrelude

import           Control.Lens              (view)
import           Data.Aeson                (ToJSON)
import           Data.Proxy                (Proxy (..))
import           Lucid
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import           Network.HTTP.ReverseProxy (ProxyDest (..),
                                            WaiProxyResponse (..), defaultOnExc,
                                            waiProxyTo)
import           Network.Wai               (Application, Request)
import           Network.Wai.Handler.Warp  (run)
import           Servant                   ((:<|>) (..), (:>), Get, JSON, Raw,
                                            Server, Tagged (..), serve)
import           Servant.HTML.Lucid

import           Configurable              (HasConfig (..))

import qualified Plugin.Logger             as Logger

import           App.Api.Config            (ApiConfig, port, proxiedHost,
                                            proxiedPort)


-- * Models

newtype Cat = Cat { cat :: String }
  deriving stock Generic
  deriving anyclass ToJSON

newtype Dog = Dog { dog :: String }
  deriving stock Generic
  deriving anyclass ToJSON


-- * API interfaces

type API
  = Get '[HTML] (Html ())
  :<|> "cat" :> Get '[JSON] Cat
  :<|> "dog" :> Get '[JSON] Dog


-- * API implementations

appServer :: Server API
appServer = pure index'
  :<|> pure Cat { cat = "mrowl" }
  :<|> pure Dog { dog = "zzzzzzzzzzzz" }
  where
    index' = p_ $ do
      "You can get either a "
      a_ [href_ "cat"] "cat"
      " or a "
      a_ [href_ "dog"] "dog"
      "."

-- * Wrapping API interface

type API'
  = API :<|> Raw


createProxyServer
  :: (HasConfig env ApiConfig, MonadReader env m, MonadIO m)
  => m Application
createProxyServer = do
  host' <- view $ setting @_ @ApiConfig . proxiedHost
  port' <- view $ setting @_ @ApiConfig . proxiedPort
  liftIO $
    waiProxyTo (forwardRequest host' port') defaultOnExc <$> newManager defaultManagerSettings
  where
    forwardRequest :: Text -> Int -> Request -> IO WaiProxyResponse
    forwardRequest host' port' _ = pure $ WPRProxyDest $ ProxyDest (encodeUtf8 host') port'

start
  :: (HasConfig env ApiConfig, HasConfig env Logger.Config, MonadReader env m, MonadIO m)
  => m ()
start = do
  proxyServer <- createProxyServer
  port' <- view $ setting @_ @ApiConfig . port
  Logger.info $ "Server is up at localhost:" <> tshow port'
  liftIO $ run port' $ serve (Proxy @API') $ appServer :<|> Tagged proxyServer
