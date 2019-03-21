{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Predictor
  ( Predictor (..)
  , PredictorId
  )
where

import ClassyPrelude

import Data.Extensible (deriveIsRecord)

import Model.Entities


deriveIsRecord ''Predictor
