module Model.User where

import Data.Set (Set)
import DateTime
import WithID (ID)

data Role = Admin | Client | Operator deriving (Show, Eq, Ord)

strToRole :: String -> Maybe Role
strToRole "admin" = Just Admin
strToRole "client" = Just Client
strToRole "operator" = Just Operator
strToRole _ = Nothing

data User
  = User
      { email :: String,
        name :: String,
        password :: String,
        roles :: Set Role,
        team :: ID,
        created :: DateTime
      }
  deriving (Show)
