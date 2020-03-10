module Time where

import Data.Aeson
import Data.Aeson.Types (prependFailure)
import Data.Scientific (toBoundedInteger)
import Data.UnixTime
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Foreign.C.Types (CTime (..))

newtype Time = Time UnixTime deriving (Show, Eq, Ord)

instance FromJSON Time where
  parseJSON =
    withScientific "UnixTime" $ \v ->
      maybe
        (prependFailure "Can't parse time, " (fail $ "incorrect argument: " ++ show v))
        (return . Time . flip UnixTime 0 . CTime)
        $ toBoundedInteger v

instance ToJSON Time where
  toJSON (Time (UnixTime (CTime sec) _)) = Number . fromIntegral $ sec

instance FromField Time where
  fromField f = case contents of
    SQLInteger n -> return . Time . flip UnixTime 0 . CTime $ n
    _ -> returnError Incompatible f "Expected integer"
    where
      contents = fieldData f

instance ToField Time where
  toField (Time (UnixTime (CTime sec) _)) = toField sec
