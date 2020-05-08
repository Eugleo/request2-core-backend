{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.WithID where

import Control.Applicative (empty)
import Control.Monad.Trans.State.Strict (get, modify)
import Data.Aeson
import Data.Proxy (Proxy (..))
import Database.Selda
import Database.Selda.SqlRow (ResultReader (..))
import Database.Selda.SqlType
import Model.IdInstances ()

instance SqlRow a => SqlRow (WithID a) where
  nestedCols _ = 1 + nestedCols (Proxy :: Proxy a)
  nextResult = do
    cs <- R get
    R $ modify tail
    case head cs of
      SqlInt n -> WithID (toId n) <$> nextResult
      x -> error $ "First column wasn't an ID, but: " ++ show x

data WithID a = WithID (ID (WithID a)) a
  deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (WithID a) where
  parseJSON (Object v) = WithID <$> v .: "id" <*> v .: "data"
  parseJSON _ = empty

instance ToJSON a => ToJSON (WithID a) where
  toJSON (WithID i a) = object ["id" .= i, "data" .= toJSON a]
