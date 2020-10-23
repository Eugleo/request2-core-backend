module Api.Query where

import Data.Model.DateTime
import Data.Text (Text)

newtype Query = Conjunction [Clause]
  deriving (Eq, Show)

data Clause
  = Extant Entity
  | Negated Entity
  deriving (Eq, Show)

data Entity
  = QualifiedDate Text [Delimited DateTime]
  | QualifiedText Text [Text]
  | QualifiedNumber Text [Delimited Integer]
  | Literal Text
  deriving (Eq, Show)

data Delimited a
  = LessThan a
  | GreaterThan a
  | LessOrEq a
  | GreaterOrEq a
  | Between a a
  | Equal a
  deriving (Eq, Show)
