module UserInfo where

import Data.Text (Text)
import Model.User (Role)
import WithID (ID)

type APIKey = Text

data UserInfo
  = UserInfo
      { userID :: ID,
        apiKey :: APIKey,
        roles :: [Role]
      }
