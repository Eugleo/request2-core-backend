{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils.Id.IdInstances where

import Database.Selda
import Web.Scotty (Parsable (..))


instance Parsable (ID a) where
    parseParam = fmap toId . parseParam


instance Read (ID a) where
    readsPrec = readsPrec
