module Main where

import Control.Monad
import Database.Selda.PostgreSQL
import Database.Table
import Server.Config
import Server.Server

main :: IO ()
main = do
  cfg <- getConfig
  withPostgreSQL (connInfo cfg) setup
  void $ server cfg
