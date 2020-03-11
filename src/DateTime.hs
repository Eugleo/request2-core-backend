module DateTime where

import Data.Aeson
import Data.Aeson.Types (prependFailure)
import Data.Scientific (toBoundedInteger)
import Data.UnixTime
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Foreign.C.Types (CTime (..))

newtype DateTime = DateTime UnixTime deriving (Show, Eq, Ord)

instance FromJSON DateTime where
  parseJSON =
    withScientific "UnixTime" $ \v ->
      maybe
        (prependFailure "Can't parse time, " (fail $ "incorrect argument: " ++ show v))
        (return . DateTime . flip UnixTime 0 . CTime)
        $ toBoundedInteger v

instance ToJSON DateTime where
  toJSON (DateTime (UnixTime (CTime sec) _)) = Number . fromIntegral $ sec

instance FromField DateTime where
  fromField f = case contents of
    SQLInteger n -> return . DateTime . flip UnixTime 0 . CTime $ n
    _ -> returnError Incompatible f "Expected integer"
    where
      contents = fieldData f

instance ToField DateTime where
  toField (DateTime (UnixTime (CTime sec) _)) = toField sec
