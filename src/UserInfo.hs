module UserInfo where

import Model.User (Role)
import WithID (ID)

type APIKey = String

data UserInfo
  = UserInfo
      { userID :: ID,
        apiKey :: APIKey,
        roles :: [Role]
      }
