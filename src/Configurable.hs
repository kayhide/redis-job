{-# LANGUAGE TypeFamilyDependencies #-}
module Configurable where

import           ClassyPrelude

class Configurable a where
  type Setting (a :: *) = r | r -> a
  type Running (a :: *) = r | r -> a

  ready :: IO (Setting a)

  activate :: Setting a -> IO (Running a)

  start :: IO (Setting a, Running a)
  start = do
    setting' <- ready
    running' <- activate setting'
    pure (setting', running')
