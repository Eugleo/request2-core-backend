{-# LANGUAGE OverloadedLabels #-}

module Database.Table where

import Data.Char (isLower)
import Data.Maybe (fromMaybe)
import Data.Model.Ann (Ann)
import Data.Model.ApiKey (ApiKey)
import Data.Model.Property (Property)
import Data.Model.Request (Request)
import Data.Model.Team (Team)
import Data.Model.User (User)
import qualified Data.Text as T
import Database.Selda

setup :: MonadSelda m => m ()
setup = do
  tryCreateTable teams
  tryCreateTable users
  tryCreateTable requests
  tryCreateTable properties
  tryCreateTable anns
  tryCreateTable apiKeys

teams :: Table Team
teams =
  tableFieldMod
    "teams"
    [ #_id :- autoPrimary,
      (#name :+ #active) :- index
    ]
    $ toName "team"

users :: Table User
users =
  tableFieldMod
    "users"
    [ #_id :- autoPrimary,
      #teamId :- foreignKey teams #_id,
      Single #email :- index,
      (#teamId :+ #name) :- index
    ]
    $ toName "request"

requests :: Table Request
requests =
  tableFieldMod
    "requests"
    [ #_id :- autoPrimary,
      #authorId :- foreignKey users #_id,
      #teamId :- foreignKey teams #_id,
      (#authorId :+ #status :+ #dateCreated) :- index,
      (#requestType :+ #status :+ #dateCreated) :- index
    ]
    $ toName "request"

properties :: Table Property
properties =
  tableFieldMod
    "properties"
    [ #_id :- autoPrimary,
      #authorId :- foreignKey users #_id,
      #requestId :- foreignKey requests #_id,
      (#_id :+ #requestId :+ #propertyType :+ #dateAdded) :- index
    ]
    $ toName "property"

anns :: Table Ann
anns =
  tableFieldMod
    "announcements"
    [ #_id :- autoPrimary,
      #authorId :- foreignKey users #_id,
      (#active :+ #dateCreated) :- index
    ]
    $ toName "user"

apiKeys :: Table ApiKey
apiKeys =
  tableFieldMod
    "api_keys"
    [ #key :- primary,
      #userId :- foreignKey users #_id,
      Single #dateCreated :- index,
      (#userId :+ #key) :- index
    ]
    $ toName "key"

toName :: Text -> Text -> Text
toName name col
  | T.isPrefixOf "_" col = T.append name col
  | T.isPrefixOf name col =
    T.tail
      . camelToSnake
      . fromMaybe col
      $ T.stripPrefix name col
  | otherwise = camelToSnake col

camelToSnake :: Text -> Text
camelToSnake = T.intercalate "_" . go []
  where
    go acc t = if T.null t then acc else go (word t : acc) (rest t)
    word = T.takeWhile isLower
    rest = T.dropWhile isLower
