{-# LANGUAGE DeriveAnyClass #-}
module App.Api where

import           ClassyPrelude

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
      p_ [href_ "cat"] "cat"
      " or a "
      a_ [href_ "dog"] "dog"
      "."

-- * Wrapping API interface

type API'
  = API :<|> Raw

-- * Proxy server

createProxyServer :: IO Application
createProxyServer =
  waiProxyTo forwardRequest defaultOnExc <$> newManager defaultManagerSettings
  where
    forwardRequest :: Request -> IO WaiProxyResponse
    forwardRequest _ = pure $ WPRProxyDest $ ProxyDest "127.0.0.1" 5100

-- * Starting proxy server with take over APIs

startApp :: IO ()
startApp = do
  proxyServer <- createProxyServer
  run 8080 $ serve (Proxy @API') $ appServer :<|> Tagged proxyServer

