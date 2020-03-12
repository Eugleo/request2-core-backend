module Main where

import ApiServer
import Config
import Control.Monad
import Database.Schema (createDatabase)

main :: IO ()
main = do
  config <- getConfig
  createDatabase config
  void $ apiServer config
