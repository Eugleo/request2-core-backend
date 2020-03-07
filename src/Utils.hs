{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Map as M
import Data.Text.Lazy
import Network.HTTP.Types.Status
import Web.Scotty

stringParam :: Text -> ActionM Text
stringParam s = do
  d <- jsonData
  case d M.!? s of
    Nothing -> do
      status badRequest400
      text $ "missing string parameter: " `append` s
      finish
    Just d -> return d

unpack = Data.Text.Lazy.unpack

pack = Data.Text.Lazy.pack
