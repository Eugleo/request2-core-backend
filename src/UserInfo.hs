module UserInfo where

import WithID (ID)

type APIKey = String

data UserInfo
  = UserInfo
      { userID :: ID,
        apiKey :: APIKey
      }
