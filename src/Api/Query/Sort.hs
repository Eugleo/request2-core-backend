module Api.Query.Sort where

import Data.Model.Team
import Database.Selda
import Database.Selda.PostgreSQL (PG)

type Sorter a = Query PG ()

teamSorter :: Sorter Team
teamSorter =