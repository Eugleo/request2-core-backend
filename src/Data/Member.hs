{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Member where

import Data.Model.DateTime (DateTime)
import Data.Model.Team (Team)
import Data.Model.User (User)
import Database.Selda (Generic, ID, SqlRow)


data Member = Member {userId :: ID User, teamId :: ID Team}
    deriving (Show, Eq, Generic, SqlRow)
