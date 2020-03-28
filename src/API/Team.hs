module API.Team where

import Data.Aeson ((.=), object, toJSON)
import qualified Database.Team as DB
import Environment
import Network.HTTP.Types.Status
import WithID (WithID (..))

add :: EnvAction ()
add = do
  team <- jsonData
  new <- DB.add team
  json new
  status created201

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
  case ann of
    Just a -> json a
    Nothing -> status notFound404

getMany :: EnvAction ()
getMany = do
  limit <- param "limit"
  offset <- param "offset"
  teams <- DB.getMany limit offset
  json (object ["teams" .= toJSON teams])
