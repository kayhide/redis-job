{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module App.Model.Predictor
  (
  -- * Data types
    Predictor
  , PredictorId

  -- * Database operations
  , predictors
  , lookup
  , list
  , create
  , update
  , destroy
  )
where

import ClassyPrelude hiding (id, lookup)

import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Extensible (deriveIsRecord)
import Database.Selda (Attr (..), ID, MonadMask, SeldaT, SqlRow, Table,
                       autoPrimary, query, select, table)
import qualified Model.Operation as Op
import Model.Orphans ()
import Model.Timestamp


-- * Data types

data Predictor =
  Predictor
  { id           :: PredictorId
  , training_set :: !Text
  , test_set     :: !Text
  , train_net    :: !Text
  , predict_net  :: !Text
  , trained_at   :: !(Maybe UTCTime)
  , tested_at    :: !(Maybe UTCTime)
  , created_at   :: !CreatedAt
  , updated_at   :: !UpdatedAt
  }
  deriving (Eq, Show, Generic, SqlRow, FromJSON, ToJSON, Default)

type PredictorId = ID Predictor

deriveIsRecord ''Predictor


-- * Database operations

predictors :: Table Predictor
predictors = table "predictors" [#id :- autoPrimary]

list :: (MonadIO m, MonadMask m) => SeldaT m [Predictor]
list = query $ select predictors

lookup
  :: (MonadIO m, MonadMask m)
  => PredictorId
  -> SeldaT m (Maybe Predictor)
lookup = Op.lookupOn predictors

create :: (MonadIO m, MonadMask m) => Predictor -> SeldaT m Predictor
create = Op.createNowOn predictors

update :: (MonadIO m, MonadMask m) => Predictor -> SeldaT m Predictor
update = Op.updateNowOn predictors

destroy :: (MonadIO m, MonadMask m) => PredictorId -> SeldaT m ()
destroy = Op.destroyOn predictors
