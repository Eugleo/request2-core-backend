{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.CommentWithoutId where

import Data.Aeson
import Data.Model.DateTime
import Data.Model.Request
import Data.Model.User
import Database.Selda


-- TODO Add private properties (can only be viewed by author)
data CommentWithoutId = Comment
    { requestId :: ID Request,
      authorId :: ID User,
      content :: Text,
      dateAdded :: DateTime
    }
    deriving (Show, Eq, Generic, FromJSON, SqlRow)
