module Model.User where

data Role = Admin | Basic | Operator deriving (Show)

type ID = String

data User
  = User
      { email :: String,
        name :: String,
        password :: String,
        roles :: [Role],
        team :: ID
      }
  deriving (Show)
