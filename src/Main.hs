module Main where

import ApiServer
import Config
import Control.Monad
import Database.General (createDatabase)

main :: IO ()
main = do
  config <- getConfig
  createDatabase config
  void $ apiServer config
