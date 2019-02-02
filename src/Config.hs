{-# LANGUAGE TypeFamilyDependencies #-}
module Config where

import           ClassyPrelude

class Configurable a where
  type Setting (a :: *) = r | r -> a
  type Running (a :: *) = r | r -> a

  build :: IO (Setting a)

  boot :: Setting a -> IO (Running a)
