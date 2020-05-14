{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.BasicAuth where

import Data.Maybe
import Data.Text ()
import Database.Selda
import qualified Database.Table as Table
import Environment
import Model.Role (Role)
import Model.User (User)
import UserInfo (UserInfo (UserInfo))

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
