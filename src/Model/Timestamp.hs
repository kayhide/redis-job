module Model.Timestamp
  ( CreatedAt (..)
  , UpdatedAt (..)
  )
where

import ClassyPrelude

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Default (Default (..))
import Database.Selda (SqlType (..))
import qualified Database.Selda.SqlType as Selda


newtype CreatedAt = CreatedAt UTCTime
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

instance Default CreatedAt where
  def = CreatedAt $ UTCTime (fromGregorian 1970 1 1) 0

instance Selda.SqlType CreatedAt where
  mkLit (CreatedAt x) = Selda.LCustom $ mkLit x
  sqlType _ = Selda.TDateTime
  fromSql = CreatedAt . Selda.fromSql
  defaultValue = mkLit (def @CreatedAt)


newtype UpdatedAt = UpdatedAt UTCTime
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

instance Default UpdatedAt where
  def = UpdatedAt $ UTCTime (fromGregorian 1970 1 1) 0

instance Selda.SqlType UpdatedAt where
  mkLit (UpdatedAt x) = Selda.LCustom $ mkLit x
  sqlType _ = Selda.TDateTime
  fromSql = UpdatedAt . Selda.fromSql
  defaultValue = mkLit (def @UpdatedAt)
