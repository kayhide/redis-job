{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Orphans where

import ClassyPrelude

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Default (Default (..))
import Database.Selda (ID, fromId, toId)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))


instance ToJSON (ID a) where
  toJSON = toJSON . fromId

instance FromJSON (ID a) where
  parseJSON = fmap toId . parseJSON

instance ToHttpApiData (ID a) where
  toUrlPiece = toUrlPiece . fromId

instance FromHttpApiData (ID a) where
  parseUrlPiece = fmap toId . parseUrlPiece

instance Default (ID a) where
  def = toId def


instance Default Text where
  def = ""
