{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Model.Role where

import Data.Aeson
import Data.Aeson.Types (prependFailure)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Database.Selda (def)
import Database.Selda.SqlType
import Text.Read (readMaybe)

data Role = Client | Operator | Admin
  deriving (Show, Read, Eq, Enum, Ord, Bounded, SqlType)

instance FromJSON Role where
  parseJSON =
    withText "Role field" $ \v ->
      case readMaybe $ T.unpack v of
        Just role -> return role
        Nothing -> prependFailure "Can't parse role, " (fail $ "incorrect argument: " ++ show v)

instance ToJSON Role where
  toJSON = String . T.pack . show

instance SqlType [Role] where
  mkLit = LCustom TText . LText . T.intercalate "," . map (T.pack . show)
  sqlType _ = TText
  fromSql (SqlString s) = mapMaybe (readMaybe . T.unpack) $ T.splitOn "," s
  fromSql v = error $ "fromSql: Role column with non-text value: " ++ show v
  defaultValue = LCustom TText $ LText def
