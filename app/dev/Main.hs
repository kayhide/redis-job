module Main where

import           ClassyPrelude

import qualified App.Api       as Api

import qualified Config


main :: IO ()
main = do
  config <- Config.activate'
  runReaderT (Api.start config) config


