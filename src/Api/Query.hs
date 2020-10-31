{-# LANGUAGE DeriveTraversable #-}

module Api.Query where

import Data.Text (Text)

newtype QuerySpecification = Conjunction [Clause]
  deriving (Eq, Show)

data Clause
  = Extant Entity
  | Negated Entity
  deriving (Eq, Show)

data Entity = Qualified Text [Delimited Text] | Literal Text
  deriving (Eq, Show)

data Delimited a
  = LessThan a
  | GreaterThan a
  | LessOrEq a
  | GreaterOrEq a
  | Between a a
  | Equal a
  deriving (Eq, Show, Functor, Foldable, Traversable)
