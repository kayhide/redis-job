module App.Handler.Predictors where

import           ClassyPrelude

import           Database.Persist (Entity, selectList)
import           Servant          ((:<|>) (..), (:>), Get, JSON, Raw, ServerT,
                                   Tagged (..), serve)

import           Configurable     (HasConfig)

import           App.Api.Config   (AppM)
import           Model.Predictor
import qualified Plugin.Db        as Db

type API
  = Get '[JSON] [Entity Predictor]

handlers
  :: (HasConfig env Db.Config)
  => ServerT API (AppM env)
handlers = index'


index'
  :: (HasConfig env Db.Config)
  => AppM env [Entity Predictor]
index' =
  Db.run $ selectList [] []
