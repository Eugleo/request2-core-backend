{-# LANGUAGE OverloadedStrings #-}

module Server.Capability where

import Data.String

capabilityList :: IsString s => [s]
capabilityList = ["request2"]
