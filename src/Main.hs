module Main where

import ApiServer
import Config
import Control.Monad
import UserDB

main :: IO ()
main = do
  config <- getConfig
  createDB config
  void $ apiServer config
