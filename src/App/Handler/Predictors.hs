{-# LANGUAGE OverloadedLabels #-}
module App.Handler.Predictors where

import ClassyPrelude

import App.Api.Config (AppM)
import Configurable (ToConfig)
import Control.Lens ((&), (.~))
import Data.Extensible
import Database.Persist (Entity (..), getJust, getJustEntity, insertEntity,
                         replace, selectList)
import Model.Predictor
import qualified Plugin.Db as Db
import Servant ((:<|>) (..), (:>), Capture, Get, JSON, Patch, Post, ReqBody,
                ServerT)



type PredictorCreating =
  Record
  '[ "predictorTrainingSet" >: Text
   , "predictorTrainNet"    >: Text
   , "predictorTestSet"     >: Text
   , "predictorPredictNet"  >: Text
   ]

type PredictorUpdatingFields =
  '[ "predictorTrainingSet" >: Text
   , "predictorTrainNet"    >: Text
   , "predictorTestSet"     >: Text
   , "predictorPredictNet"  >: Text
   ]

type PredictorUpdating =
  Record PredictorUpdatingFields

type API
  = Get '[JSON] [Entity Predictor]
  :<|> ReqBody '[JSON] PredictorCreating :> Post '[JSON] (Entity Predictor)
  :<|> Capture "predictorId" PredictorId :> Get '[JSON] (Entity Predictor)
  :<|> Capture "predictorId" PredictorId :> ReqBody '[JSON] PredictorUpdating :> Patch '[JSON] (Entity Predictor)

handlers
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => ServerT API (AppM env)
handlers = index' :<|> create' :<|> show' :<|> update'


index'
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => AppM env [Entity Predictor]
index' =
  Db.run $ selectList [] []


create'
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => PredictorCreating -> AppM env (Entity Predictor)
create' creating' = do
  now <- liftIO getCurrentTime
  let item'' = shrink
        $ #predictorTrainedAt @= (Nothing :: Maybe UTCTime)
        <: #predictorTestedAt @= (Nothing :: Maybe UTCTime)
        <: #predictorCreatedAt @= now
        <: #predictorUpdatedAt @= now
        <: creating'

  Db.run $ insertEntity (fromRecord item'')


show'
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => PredictorId -> AppM env (Entity Predictor)
show' id' =
  Db.run $ getJustEntity id'


update'
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => PredictorId -> PredictorUpdating -> AppM env (Entity Predictor)
update' id' updating' = do
  item' <- toRecord <$> Db.run (getJust id')
  now <- liftIO getCurrentTime

  let item'' =
        hfoldlWithIndex apply' item' (wrench updating')
        & #predictorUpdatedAt .~ now
  let record' = fromRecord item''
  Db.run $ replace id' record'
  pure (Entity id' record')

apply' :: Membership xs x -> Record xs -> Nullable (Field Identity) x -> Record xs
apply' m item' (Nullable f) = maybe id (pieceAt m .~) f item'
