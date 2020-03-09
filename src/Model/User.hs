module Model.User where

import Data.Set (Set)

data Role = Admin | Basic | Operator deriving (Show)

type ID = String

data User
  = User
      { email :: String,
        name :: String,
        password :: String,
        roles :: Set Role,
        team :: ID
      }
  deriving (Show)
