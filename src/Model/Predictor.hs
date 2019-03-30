{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
module Model.Predictor where

import ClassyPrelude hiding (id)

import Data.Time.LocalTime
import Data.Aeson (ToJSON)
import Data.Default (Default (..))
import Data.Extensible (deriveIsRecord)
import Database.Beam
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import Plugin.Db ()



data PredictorT f =
  Predictor
  { _id           :: Columnar f Int
  , _training_set :: Columnar f Text
  , _test_set     :: Columnar f Text
  , _train_net    :: Columnar f Text
  , _predict_net  :: Columnar f Text
  , _trained_at   :: Columnar f (Maybe LocalTime)
  , _tested_at    :: Columnar f (Maybe LocalTime)
  , _created_at   :: Columnar f LocalTime
  , _updated_at   :: Columnar f LocalTime
  }
  deriving (Generic, Beamable)

type Predictor = PredictorT Identity
deriving instance Eq Predictor
deriving instance Show Predictor
deriving instance ToJSON Predictor

type PredictorId = PrimaryKey PredictorT Identity
deriving instance Eq PredictorId
deriving instance Show PredictorId
deriving instance ToJSON PredictorId


instance ToHttpApiData PredictorId where
  toUrlPiece (PredictorId id') = toUrlPiece id'

instance FromHttpApiData PredictorId where
  parseUrlPiece = fmap PredictorId . parseUrlPiece

instance Table PredictorT where
   data PrimaryKey PredictorT f = PredictorId (Columnar f Int)
     deriving (Generic, Beamable)

   primaryKey = PredictorId . _id
