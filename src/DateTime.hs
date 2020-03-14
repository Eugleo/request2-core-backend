module DateTime where

import Data.Aeson
import Data.Aeson.Types (prependFailure)
import Data.Scientific (toBoundedInteger)
import Data.UnixTime
import Data.Int (Int64)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Foreign.C.Types (CTime (..))

newtype DateTime = DateTime Int64 deriving (Show, Eq, Ord)

instance FromJSON DateTime where
  parseJSON =
    withScientific "UnixTime" $ \v ->
      maybe
        (prependFailure "Can't parse time, " $
         fail $ "incorrect argument: " ++ show v)
        (return . DateTime)
        $ toBoundedInteger v

instance ToJSON DateTime where
  toJSON (DateTime sec) = Number . fromIntegral $ sec

instance FromField DateTime where
  fromField f = case contents of
    SQLInteger n -> return $ DateTime n
    _ -> returnError Incompatible f "Expected integer"
    where
      contents = fieldData f

instance ToField DateTime where
  toField (DateTime sec) = toField sec

now :: IO DateTime
now = do CTime t <- utSeconds <$> getUnixTime
         return $ DateTime t
