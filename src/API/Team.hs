module API.Team where

import Data.Aeson ((.=), object, toJSON)
import qualified Database.Team as DB
import Environment
import WithID (WithID (..))

add :: EnvAction ()
add = do
  team <- jsonData
  new <- DB.add team
  json new
  envCreated

edit :: EnvAction ()
edit = do
  teamID <- param "team_id"
  team <- jsonData
  DB.edit $ WithID teamID team

deactivate :: EnvAction ()
deactivate = do
  teamID <- param "team_id"
  DB.deactivate teamID

get :: EnvAction ()
get = do
  annID <- param "team_id"
  ann <- DB.get annID
  maybe envNotFound json ann

getMany :: EnvAction ()
getMany = do
  limit <- param "limit"
  offset <- param "offset"
  teams <- DB.getMany limit offset
  total <- DB.count
  json (object ["teams" .= toJSON teams, "total" .= total])
