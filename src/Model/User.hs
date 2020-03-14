module Model.User where

import DateTime
import WithID (ID)

data Role = Client | Operator | Admin deriving (Show, Read, Eq, Ord)

data User
  = User
      { email :: String,
        name :: String,
        password :: String,
        roles :: [Role],
        team :: ID,
        created :: DateTime
      }
  deriving (Show)
