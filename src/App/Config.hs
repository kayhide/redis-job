{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module App.Config where

import           ClassyPrelude

import           Control.Lens          ((^.), _1, _2)

import           Configurable
import           Data.Extensible
import qualified Plugin.Db.Config      as Db
import qualified Plugin.Redis.Config   as Redis
import qualified Plugin.Sidekiq.Config as Sidekiq


type AppConfig = (AppSetting, AppRunning)

type AppSetting = Record
  '[ "db" ':> Db.DbSetting
   , "redis" ':> Redis.RedisSetting
   , "sidekiq" ':> Sidekiq.SidekiqSetting
   ]

type AppRunning = Record
  '[ "db" ':> Db.DbRunning
   , "redis" ':> Redis.RedisRunning
   , "sidekiq" ':> Sidekiq.SidekiqRunning
   ]

instance Configurable AppConfig where
  type Name AppConfig = "app"
  type Setting AppConfig = AppSetting
  type Running AppConfig = AppRunning
  type Deps AppConfig = '[]

  ready = do
    db' <- ready
    redis' <- ready
    sidekiq' <- ready
    pure $ #db @= db'
      <: #redis @= redis'
      <: #sidekiq @= sidekiq'
      <: nil

  start setting' _ = do
    let conf' = (setting', nil)
    db' <- start (setting' ^. itemAssoc (Proxy @(Name Db.DbConfig))) conf'

    let conf'' = second (#db @= db' <:) conf'
    redis' <- start (setting' ^. itemAssoc (Proxy @(Name Redis.RedisConfig))) conf''

    let conf''' = second (#redis @= redis' <:) conf''
    sidekiq' <- start (setting' ^. itemAssoc (Proxy @(Name Sidekiq.SidekiqConfig))) conf'''

    pure . snd $ second (shrink . (#sidekiq @= sidekiq' <:)) conf'''


instance ( Configurable a
         , Associate (Name a) (Setting a) settings
         , Associate (Name a) (Running a) runnings
         ) => HasConfig (Record settings, Record runnings) a where
  setting = _1 . itemAssoc (Proxy @(Name a))
  running = _2 . itemAssoc (Proxy @(Name a))
