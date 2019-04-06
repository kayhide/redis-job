{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Entities where

import ClassyPrelude

import Database.Persist.Postgresql
import Database.Persist.TH

share [mkPersist sqlSettings, mkSave "modelEntities"] [persistLowerCase|
Predictor json sql=predictors
    trainingSet Text
    trainNet    Text
    testSet     Text
    predictNet  Text
    trainedAt   UTCTime Maybe
    testedAt    UTCTime Maybe
    createdAt   UTCTime
    updatedAt   UTCTime
    deriving Eq
    deriving Show
|]
