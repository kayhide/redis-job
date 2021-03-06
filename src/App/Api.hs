{-# LANGUAGE DeriveAnyClass #-}
module App.Api
  ( start
  , AppM
  )
where

import ClassyPrelude hiding (Handler)

import App.Api.Config (ApiConfig, AppM (..), port, proxiedHost, proxiedPort)
import qualified App.Handler.Predictors as Predictors
import Configurable (ToConfig, setting)
import Control.Lens (view)
import Control.Monad.Except (ExceptT (..))
import Data.Extensible
import Data.Proxy (Proxy (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..),
                                  defaultOnExc, waiProxyTo)
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp (run)
import qualified Plugin.Db as Db
import qualified Plugin.Logger as Logger
import Servant ((:<|>) (..), (:>), Handler (..), Raw, ServerT, Tagged (..),
                hoistServer, serve)

-- * API interfaces

type API
  = "api" :> ("predictors" :> Predictors.API)


-- * API implementations

appServer
  :: ( Member xs ApiConfig
     , Member xs Logger.Config
     , Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => ServerT API (AppM env)
appServer = Predictors.handlers

-- * Wrapping API interface

type API'
  = API :<|> Raw


createProxyServer
  :: ( Member xs ApiConfig
     , Member xs Logger.Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
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
  :: forall xs env m.
     ( Member xs ApiConfig
     , Member xs Logger.Config
     , Member xs Db.Config
     , env ~ (ToConfig :* xs)
     , MonadReader env m
     , MonadIO m
     )
  => env
  -> m ()
start env = do
  proxyServer <- createProxyServer
  port' <- view $ setting @_ @ApiConfig . port
  Logger.info $ "Server is up at localhost:" <> tshow port'
  let api = Proxy @API
  let api' = Proxy @API'
  liftIO $ run port' $ serve api' $ hoistServer api nt (appServer @xs) :<|> Tagged proxyServer

  where
    nt :: AppM env a -> Handler a
    nt action = Handler $ ExceptT $ try $ runReaderT (unAppM action) env
