{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module App.Job.TrainJob where

import ClassyPrelude

import Control.Concurrent (threadDelay)
import Data.Aeson

import Plugin.Sidekiq.Job


-- * Sample of enqued job json
-- {
--   "class": "ActiveJob::QueueAdapters::SidekiqAdapter::JobWrapper",
--   "wrapped": "TrainJob",
--   "queue": "default",
--   "args": [
--     {
--       "job_class": "TrainJob",
--       "job_id": "2e5d4b72-0d6a-4423-b4c6-16314a006370",
--       "provider_job_id": null,
--       "queue_name": "default",
--       "priority": null,
--       "arguments": [
--         {
--           "_aj_globalid": "gid://rb/Predictor/1"
--         }
--       ],
--       "executions": 0,
--       "locale": "en"
--     }
--   ],
--   "retry": true,
--   "jid": "24577260afdd1fd504948afd",
--   "created_at": 1549087800.610423,
--   "enqueued_at": 1549087800.610476
-- }


data TrainJob

data TrainJobArgs = TrainJobArgs { _aj_globalid :: Text }
  deriving (Eq, Show, Generic)
  deriving (FromJSON)

instance Job TrainJob where
  type Args TrainJob = TrainJobArgs
  perform args = do
    putStrLn "Starting TrainJob: "
    print args
    threadDelay 1000000
    putStrLn "Training..."
    threadDelay 1000000
    putStrLn "Still training..."
    threadDelay 1000000
    putStrLn "Almost done..."
    threadDelay 1000000
    putStrLn "Finished!"
