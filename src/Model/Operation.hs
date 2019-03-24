{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
module Model.Operation
where

import ClassyPrelude

import Control.Lens ((&), (.~), (^.))
import Data.Extensible (AssocValue, Field (..), Forall, IsRecord,
                        Membership (..), RecFields, getMemberId,
                        hfoldMapWithIndexFor, record)
import Data.Generics.Product (HasField', field')
import Data.Proxy (Proxy (..))
import Database.Selda (Assignment (..), ID, IsLabel, MonadMask, Relational,
                       SeldaT, Selector, Table, literal, query, restrict, deleteFrom,
                       select, with, (!), (.==))
import qualified Database.Selda as Selda
import qualified Database.Selda.Unsafe as Selda
import Model.Timestamp


lookupOn
  :: ( Relational a
     , IsLabel "id" (Selector a (ID a))
     , MonadIO m
     , MonadMask m
     )
  => Table a
  -> ID a
  -> SeldaT m (Maybe a)
lookupOn table' id' =
  fmap headMay $ query $ do
    row' <- select table'
    restrict $ row' ! #id .== literal id'
    pure row'


createNowOn
  :: forall a m.
     ( Relational a
     , HasField' "id" a (ID a)
     , HasField' "created_at" a CreatedAt
     , HasField' "updated_at" a UpdatedAt
     , MonadIO m
     , MonadMask m
     )
  => Table a
  -> a
  -> SeldaT m a
createNowOn table' item' = do
  now <- liftIO getCurrentTime
  let item'' = item'
        & field' @"id" .~ Selda.def
        & field' @"created_at" .~ CreatedAt now
        & field' @"updated_at" .~ UpdatedAt now
  id' <- Selda.insertWithPK table' $ [item'']
  pure $ item'' & field' @"id" .~ id'


class (Selda.SqlType (AssocValue kv)) => SqlTypeAssoc kv
instance (Selda.SqlType (AssocValue kv)) => SqlTypeAssoc kv

updateNowOn
  :: forall a m.
     ( Relational a
     , Forall SqlTypeAssoc (RecFields a)
     , IsLabel "id" (Selector a (ID a))
     , HasField' "id" a (ID a)
     , HasField' "updated_at" a UpdatedAt
     , IsRecord a
     , MonadIO m
     , MonadMask m
     )
  => Table a -> a -> SeldaT m a
updateNowOn table' item' = do
  now <- liftIO getCurrentTime
  let item'' = item'
        & field' @"updated_at" .~ UpdatedAt now
  void $ Selda.update table'
    (\row' -> row' ! #id .== (literal $ item'' ^. field' @"id"))
    (\row' -> row' `with`
        hfoldMapWithIndexFor (Proxy @SqlTypeAssoc) f (item'' ^. record)
    )
  pure item''
  where
    f :: forall kv v s. (v ~ AssocValue kv, Selda.SqlType v)
      => Membership (RecFields a) kv -> Field Identity kv -> [Assignment s a]
    f m (Field (Identity x)) = [Selda.unsafeSelector (getMemberId m) := literal x]


destroyOn
  :: ( Relational a
     , IsLabel "id" (Selector a (ID a))
     , MonadIO m
     , MonadMask m
     )
  => Table a
  -> ID a
  -> SeldaT m ()
destroyOn table' id' =
    void $ deleteFrom table'
    (\row' -> row' ! #id .== literal id')
