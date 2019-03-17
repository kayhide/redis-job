{-# LANGUAGE DeriveAnyClass #-}
module App.Api
  ( start
  , AppM
  )
where

import ClassyPrelude hiding (Handler)

import Control.Lens (view)
import Control.Monad.Except (ExceptT (..))
import Data.Proxy (Proxy (..))
import Lucid
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..),
                                  defaultOnExc, waiProxyTo)
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp (run)
import Servant ((:<|>) (..), (:>), Get, Handler (..), JSON, Raw, Server,
                ServerT, Tagged (..), hoistServer, serve)
import Servant.HTML.Lucid

import Configurable (HasConfig (..))

import qualified Plugin.Db as Db
import qualified Plugin.Logger as Logger

import App.Api.Config (ApiConfig, AppM (..), port, proxiedHost, proxiedPort)

import qualified App.Handler.Predictors as Predictors

-- * API interfaces

type API
  = Get '[HTML] (Html ())
  :<|> ("predictors" :> Predictors.API)


-- * API implementations

appServer
  :: forall env.
     (HasConfig env ApiConfig,
      HasConfig env Logger.Config,
      HasConfig env Db.Config
     )
  => ServerT API (AppM env)
appServer = pure index'
  :<|> Predictors.handlers
  where
    index' = p_ $ do
      "You can get either a "
      a_ [href_ "predictors"] "predictors"
      "."

-- * Wrapping API interface

type API'
  = API :<|> Raw


createProxyServer
  :: (HasConfig env ApiConfig,
      HasConfig env Logger.Config,
      MonadReader env m, MonadIO m)
  => m Application
createProxyServer = do
  host' <- view $ setting @_ @ApiConfig . proxiedHost
  port' <- view $ setting @_ @ApiConfig . proxiedPort
  Logger.info $ "Proxy is on " <> host' <> ":" <> tshow port'
  liftIO $ do
    waiProxyTo (forwardRequest host' port') defaultOnExc <$> newManager defaultManagerSettings
  where
    forwardRequest :: Text -> Int -> Request -> IO WaiProxyResponse
    forwardRequest host' port' _ = pure $ WPRProxyDest $ ProxyDest (encodeUtf8 host') port'


start
  :: forall env m.
     (HasConfig env ApiConfig,
      HasConfig env Logger.Config,
      HasConfig env Db.Config,
      MonadReader env m, MonadIO m)
  => env
  -> m ()
start env = do
  proxyServer <- createProxyServer
  port' <- view $ setting @_ @ApiConfig . port
  Logger.info $ "Server is up at localhost:" <> tshow port'
  let api = Proxy @API
  let api' = Proxy @API'
  liftIO $ run port' $ serve api' $ hoistServer api nt appServer :<|> Tagged proxyServer

  where
    nt :: AppM env a -> Handler a
    nt action = Handler $ ExceptT $ try $ runReaderT (unAppM action) env
