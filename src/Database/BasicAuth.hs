{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.BasicAuth where

import Data.Environment
import Data.Maybe
import Data.Model.Role (Role)
import Data.Model.User (User)
import Data.Text ()
import Data.UserInfo (UserInfo (UserInfo))
import Database.Selda
import qualified Database.Table as Table

findApiKeyUser :: Text -> EnvAction (Maybe UserInfo)
findApiKeyUser key = do
  res <- query $ do
    user <- select Table.users
    apiKey <- innerJoin (\k -> k ! #userId .== user ! #_id) $ do
      apiKey <- select Table.apiKeys
      restrict (apiKey ! #key .== literal key)
      return apiKey
    return (user ! #_id :*: apiKey ! #key :*: user ! #roles)
  return $ mkUserInfo <$> listToMaybe res

mkUserInfo :: (ID User :*: Text :*: [Role]) -> UserInfo
mkUserInfo (uid :*: ak :*: rs) = UserInfo uid ak rs
