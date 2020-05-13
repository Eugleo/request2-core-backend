{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.BasicAuth where

import Data.Maybe
import Data.Text ()
import qualified Database.Schema as DB
import Database.Selda
import Environment
import Model.Role (Role)
import Model.User (User)
import UserInfo

findApiKeyUser :: Text -> EnvAction (Maybe UserInfo)
findApiKeyUser key = do
  res <- lift . lift $ query (q key)
  return $ mkUserInfo <$> listToMaybe res

q key = do
  user <- select DB.users
  ak <- innerJoin (\k -> k ! #_id .== user ! #_id) aks
  return (user ! #_id :*: ak ! #key :*: user ! #roles)
  where
    aks = do
      k <- select DB.apiKeys
      restrict (k ! #key .== literal key)
      return k

mkUserInfo :: (ID User :*: Text :*: [Role]) -> UserInfo
mkUserInfo (uid :*: ak :*: rs) = UserInfo uid ak rs
