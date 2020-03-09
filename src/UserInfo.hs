module UserInfo where

type APIKey = String

type ID = String

data UserInfo
  = UserInfo
      { id :: ID,
        apiKey :: APIKey
      }
