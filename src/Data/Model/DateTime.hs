{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Model.DateTime where

import Data.Aeson
import Data.Aeson.Types (prependFailure)
import Data.Int (Int64)
import Data.Scientific (toBoundedInteger)
import Data.Text.Encoding (encodeUtf8)
import Data.UnixTime
import Database.Selda
import Database.Selda.SqlType
import Foreign.C.Types (CTime (..))

newtype DateTime = DateTime Int64
  deriving (Show, Eq, Ord, Num)

instance SqlType DateTime where
  mkLit (DateTime t) = LCustom TInt . LInt . fromIntegral $ t
  sqlType _ = TInt
  fromSql (SqlInt s) = DateTime $ fromIntegral s
  fromSql v = error $ "fromSql: DateTime column with non-int value: " ++ show v
  defaultValue = LCustom TInt $ LInt def

instance FromJSON DateTime where
  parseJSON =
    withScientific "UnixTime" $ \v ->
      maybe
        ( prependFailure "Can't parse time, " $
            fail $
              "incorrect argument: " ++ show v
        )
        (return . DateTime)
        $ toBoundedInteger v

instance ToJSON DateTime where
  toJSON (DateTime sec) = Number . fromIntegral $ sec

now :: IO DateTime
now = fromUnixTime <$> getUnixTime

fromUnixTime :: UnixTime -> DateTime
fromUnixTime = DateTime . (\(CTime t) -> t) . utSeconds

parseDateTime :: Text -> DateTime
parseDateTime = fromUnixTime . parseUnixTimeGMT "%Y-%m-%d" . encodeUtf8
