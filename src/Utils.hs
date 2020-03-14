{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Map ((!?))
import Data.Text
import Data.Text.Lazy (fromStrict)
import HTTPHelpers
import Web.Scotty

--TODO move this to Environment and convert to withParams or so
stringParam :: Text -> ActionM Text
stringParam s = do
  jsonD <-
    jsonData `rescue`
    (\msg -> do
       text ("Query JSON parsing error: " <> msg)
       finishBadRequest)
  case jsonD !? s of
    Nothing -> do
      text $ "Missing string parameter: " <> fromStrict s
      finishBadRequest
    Just d -> return d

unpack :: Text -> String
unpack = Data.Text.unpack

pack :: String -> Text
pack = Data.Text.pack
