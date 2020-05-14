module Main where

import ApiServer
import Config
import Control.Monad
import Database.Selda.PostgreSQL
import Database.Table

main :: IO ()
main = do
  config <- getConfig
  withPostgreSQL connInfo setup
  void $ apiServer config
