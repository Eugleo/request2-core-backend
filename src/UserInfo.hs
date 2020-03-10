module UserInfo where

type APIKey = String

type ID = Integer

data UserInfo
  = UserInfo
      { userID :: ID,
        apiKey :: APIKey
      }
