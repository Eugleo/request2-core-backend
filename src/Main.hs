module Main where

import ApiServer
import Config
import Control.Monad
import Database.Schema
import Database.Selda.PostgreSQL

main :: IO ()
main = do
  config <- getConfig
  withPostgreSQL connInfo setup
  void $ apiServer config
