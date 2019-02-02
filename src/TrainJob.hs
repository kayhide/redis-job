{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module TrainJob where

import           ClassyPrelude

import           Data.Aeson
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

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


-- * Abstract Job

class Job a where
  type Args a  = args | args -> a
  perform :: Args a -> IO ()

data JobArgs a = JobArgs
    { _job_class       :: Text
    , _job_id          :: Text
    , _provider_job_id :: Maybe Text
    , _queue_name      :: Text
    , _priority        :: Maybe Int
    , _arguments       :: [Args a]
    , _executions      :: Int
    , _locale          :: Text
    }
deriving instance Eq (Args a) => Eq (JobArgs a)
deriving instance Show (Args a) => Show (JobArgs a)

instance FromJSON (Args a) => FromJSON (JobArgs a) where
  parseJSON = withObject "args" $ \o ->
    JobArgs
    <$> o .: "job_class"
    <*> o .: "job_id"
    <*> o .: "provider_job_id"
    <*> o .: "queue_name"
    <*> o .: "priority"
    <*> o .: "arguments"
    <*> o .: "executions"
    <*> o .: "locale"


data JobWrapper a = JobWrapper
  { _class       :: Text
  , _wrapped     :: Text
  , _queue       :: Text
  , _args        :: [JobArgs a]
  , _retry       :: Bool
  , _jid         :: Text
  , _created_at  :: UTCTime
  , _enqueued_at :: UTCTime
  }
deriving instance Eq (Args a) => Eq (JobWrapper a)
deriving instance Show (Args a) => Show (JobWrapper a)

instance FromJSON (Args a) => FromJSON (JobWrapper a) where
  parseJSON = withObject "job" $ \o ->
    JobWrapper
    <$> o .: "class"
    <*> o .: "wrapped"
    <*> o .: "queue"
    <*> o .: "args"
    <*> o .: "retry"
    <*> o .: "jid"
    <*> (posixSecondsToUTCTime <$> o .: "created_at")
    <*> (posixSecondsToUTCTime <$> o .: "enqueued_at")


-- * Concrete Job

data TrainJob

data TrainJobArgs = TrainJobArgs { _aj_globalid :: Text }
  deriving (Eq, Show, Generic)
  deriving (FromJSON)

instance Job TrainJob where
  type Args TrainJob = TrainJobArgs
  perform args = do
    print "Performing TrainJob"
