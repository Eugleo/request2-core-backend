module Model.User where

import Data.Set (Set)

data Role = Admin | Basic | Operator deriving (Show, Eq, Ord)

strToRole :: String -> Maybe Role
strToRole "Admin" = Just Admin
strToRole "Basic" = Just Basic
strToRole "Operator" = Just Operator
strToRole _ = Nothing

type ID = Integer

data User
  = User
      { email :: String,
        name :: String,
        password :: String,
        roles :: Set Role,
        team :: ID
      }
  deriving (Show)
