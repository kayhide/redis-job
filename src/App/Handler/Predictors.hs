{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
module App.Handler.Predictors where

import ClassyPrelude hiding (delete)

import Control.Lens ((&), (.~), (^.))
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.LocalTime (LocalTime, utc, utcToLocalTime)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Query ((==.))
import Servant ((:<|>) (..), (:>), Capture, Get, JSON, Patch, Post, ReqBody,
                ServerT)
import Servant.API.Verbs (DeleteNoContent)

import Configurable (HasConfig)

import App.Api.Config (AppM)
import Model.Predictor
import qualified Plugin.Db as Db

data ApplicationDb f =
  ApplicationDb
  { _predictors :: f  (TableEntity PredictorT) }
  deriving (Generic, Database be)

applicationDb :: DatabaseSettings be ApplicationDb
applicationDb = defaultDbSettings

ApplicationDb predictors = applicationDb


data PredictorCreating =
  PredictorCreating
  { training_set :: Text
  , test_set     :: Text
  , train_net    :: Text
  , predict_net  :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PredictorUpdating =
  PredictorUpdating
  { training_set :: Text
  , test_set     :: Text
  , train_net    :: Text
  , predict_net  :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type API
  = Get '[JSON] [Predictor]
  :<|> ReqBody '[JSON] PredictorCreating :> Post '[JSON] Predictor
  :<|> Capture "predictorId" PredictorId :> Get '[JSON] Predictor
  :<|> Capture "predictorId" PredictorId :> ReqBody '[JSON] PredictorUpdating :> Patch '[JSON] Predictor
  :<|> Capture "predictorId" PredictorId :> DeleteNoContent '[JSON] ()

handlers :: (HasConfig env Db.Config) => ServerT API (AppM env)
handlers = index' :<|> create' :<|> show' :<|> update' :<|> destroy'


index'
  :: (HasConfig env Db.Config)
  => AppM env [Predictor]
index' =
  Db.run $
  runSelectReturningList $ select $ all_ predictors


create'
  :: (HasConfig env Db.Config)
  => PredictorCreating -> AppM env Predictor
create' creating'@PredictorCreating {..} = do
  now :: LocalTime <- utcToLocalTime utc <$> liftIO getCurrentTime
  fmap headEx $ Db.run $
    runInsertReturningList $ insert predictors $ insertExpressions [
    Predictor
      default_
      (val_ training_set)
      (val_ test_set)
      (val_ train_net)
      (val_ predict_net)
      (val_ Nothing)
      (val_ Nothing)
      (val_ now)
      (val_ now)
    ]


show'
  :: (HasConfig env Db.Config)
  => PredictorId -> AppM env Predictor
show' id' =
  fmap fromJust $ Db.run $
    runSelectReturningOne $ lookup_ predictors id'


update'
  :: (HasConfig env Db.Config)
  => PredictorId -> PredictorUpdating -> AppM env Predictor
update' id' updating'@PredictorUpdating {..} = do
  predictor' <- show' id'
  now :: LocalTime <- utcToLocalTime utc <$> liftIO getCurrentTime
  let predictor'' = predictor'
        { _training_set = training_set
        , _test_set = test_set
        , _train_net = train_net
        , _predict_net = predict_net
        , _updated_at = now
        } :: Predictor
  Db.run $ runUpdate $
    save predictors predictor''
  pure predictor''


destroy'
  :: (HasConfig env Db.Config)
  => PredictorId -> AppM env ()
destroy' (PredictorId id') =
  Db.run $ runDelete $ delete predictors $ \p -> _id p ==. val_ id'

