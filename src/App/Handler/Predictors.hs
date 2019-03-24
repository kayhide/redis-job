module App.Handler.Predictors where

import ClassyPrelude

import App.Api.Config (AppM)
import Configurable (ToConfig)
import Control.Lens ((%~), (&))
import Data.Default (Default (..))
import Data.Extensible
import Database.Selda ()
import Model.Conduct (Optional (..), Required (..), conduct, induct)
import Model.Predictor (Predictor, PredictorId)
import qualified Model.Predictor as Predictor
import qualified Plugin.Db as Db
import Servant ((:<|>) (..), (:>), Capture, Get, JSON, Patch, Post, ReqBody,
                ServerT)
import Servant.API.Verbs (DeleteNoContent)


type API
  = Get '[JSON] [Predictor]
  :<|> ReqBody '[JSON] CreateParams :> Post '[JSON] Predictor
  :<|> Capture "predictor_id" PredictorId :> Get '[JSON] Predictor
  :<|> Capture "predictor_id" PredictorId :> ReqBody '[JSON] UpdateParams :> Patch '[JSON] Predictor
  :<|> Capture "predictor_id" PredictorId :> DeleteNoContent '[JSON] ()

handlers
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => ServerT API (AppM env)
handlers = index' :<|> create' :<|> show' :<|> update' :<|> destroy'


index'
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => AppM env [Predictor]
index' =
  Db.run Predictor.list


type CreateParams =
  Record
  '[ "training_set" >: Required Text
   , "test_set"     >: Required Text
   , "train_net"    >: Required Text
   , "predict_net"  >: Required Text
   ]

create'
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => CreateParams -> AppM env Predictor
create' params' = do
  let item' = def :: Predictor
  let item'' = item' & record %~ induct (conduct params')
  Db.run $ Predictor.create item''


show'
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => PredictorId -> AppM env Predictor
show' id' = do
  item' <- Db.run $ Predictor.lookup id'
  maybe (fail "Not Found") pure item'


type UpdateParams =
  Record
  '[ "training_set" >: Optional Text
   , "test_set"     >: Optional Text
   , "train_net"    >: Optional Text
   , "predict_net"  >: Optional Text
   ]

update'
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => PredictorId -> UpdateParams -> AppM env Predictor
update' id' params' = do
  item' <-
    Db.run (Predictor.lookup id')
    >>= maybe (fail "Not Found") pure
  let item'' = item' & record %~ induct (conduct params')
  Db.run $ Predictor.update item''


destroy'
  :: ( Member xs Db.Config
     , env ~ (ToConfig :* xs)
     )
  => PredictorId -> AppM env ()
destroy' id' =
  Db.run $ Predictor.destroy id'
