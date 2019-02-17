{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module App.Config where

import           ClassyPrelude

import           Control.Lens          ((^.), _1, _2)
import           Control.Lens.TH

import           Configurable
import           Data.Extensible
import qualified Plugin.Db.Config      as Db
import qualified Plugin.Redis.Config   as Redis
import qualified Plugin.Sidekiq.Config as Sidekiq


type AppConfig = (AppSetting, AppRunning)

type AppSetting = Record
  '[ "db" :> Db.DbSetting
   , "redis" :> Redis.RedisSetting
   , "sidekiq" :> Sidekiq.SidekiqSetting
   ]

type AppRunning = Record
  '[ "db" :> Db.DbRunning
   , "redis" :> Redis.RedisRunning
   , "sidekiq" :> Sidekiq.SidekiqRunning
   ]

instance Configurable AppConfig where
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
    db' <- start (setting' ^. #db) ()
    redis' <- start (setting' ^. #redis) ()

    let sub = (shrink setting', (#db @= db' <: #redis @= redis' <: nil)) :: AppConfigSub
    sidekiq' <- start (setting' ^. #sidekiq) sub
    pure $ #db @= db'
      <: #redis @= redis'
      <: #sidekiq @= sidekiq'
      <: nil

instance HasConfig AppConfig Db.DbConfig where
  setting = _1 . #db
  running = _2 . #db

instance HasConfig AppConfig Redis.RedisConfig where
  setting = _1 . #redis
  running = _2 . #redis

instance HasConfig AppConfig Sidekiq.SidekiqConfig where
  setting = _1 . #sidekiq
  running = _2 . #sidekiq




type AppConfigSub = (AppSettingSub, AppRunningSub)

type AppSettingSub = Record
  '[ "db" :> Db.DbSetting
   , "redis" :> Redis.RedisSetting
   ]

type AppRunningSub = Record
  '[ "db" :> Db.DbRunning
   , "redis" :> Redis.RedisRunning
   ]

instance HasConfig AppConfigSub Db.DbConfig where
  setting = _1 . #db
  running = _2 . #db

instance HasConfig AppConfigSub Redis.RedisConfig where
  setting = _1 . #redis
  running = _2 . #redis
