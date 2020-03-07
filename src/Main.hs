module Main where

import AdminServer
import ApiServer
import Config
import Control.Concurrent.Async
import Control.Monad
import LMTP
import UserDB

main :: IO ()
main = do
  config <- getConfig
  createDB config
  void $ apiServer config
