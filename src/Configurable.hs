{-# LANGUAGE TypeFamilyDependencies #-}
module Configurable where

import           ClassyPrelude

class Configurable a where
  type Setting (a :: *) = r | r -> a
  type Running (a :: *) = r | r -> a

  ready :: IO (Setting a)

  start :: Setting a -> IO (Running a)

  activate :: IO (Setting a, Running a)
  activate = do
    setting' <- ready
    running' <- start setting'
    pure (setting', running')
