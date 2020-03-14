module Model.User where

import DateTime
import WithID (ID)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Role = Client | Operator | Admin deriving (Show, Read, Eq, Ord)

rolesToString :: [Role] -> String
rolesToString = unwords . map show

stringToRoles :: String -> [Role]
stringToRoles = mapMaybe readMaybe . words

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
