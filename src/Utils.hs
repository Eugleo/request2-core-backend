{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Map ((!?))
import Data.Text.Lazy
import Network.HTTP.Types.Status
import Web.Scotty

stringParam :: Text -> ActionM Text
stringParam s = do
  jsonD <- jsonData
  case jsonD !? s of
    Nothing -> do
      status badRequest400
      text $ "Missing string parameter: " `append` s
      finish
    Just d -> return d

unpack :: Text -> String
unpack = Data.Text.Lazy.unpack

pack :: String -> Text
pack = Data.Text.Lazy.pack
