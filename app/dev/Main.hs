module Main where

import           ClassyPrelude

import qualified App.Api          as Api

import qualified Config


main :: IO ()
main = do
  config <- Config.activate'
  say "Server is up at localhost:8080"
  Api.startApp
