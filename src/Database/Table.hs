{-# LANGUAGE OverloadedLabels #-}

module Database.Table where

import Data.Char (isUpper, toLower)
import Data.Maybe (fromMaybe)
import Data.Member (Member)
import Data.Model.Ann (Ann)
import Data.Model.ApiKey (ApiKey)
import Data.Model.Comment (Comment)
import Data.Model.Property (Property)
import Data.Model.Request (Request)
import Data.Model.SecurityToken (SecurityToken)
import Data.Model.Team (Team)
import Data.Model.User (User)
import qualified Data.Text as T
import Database.Selda


createAll :: MonadSelda m => m ()
createAll = do
    tryCreateTable teams
    tryCreateTable users
    tryCreateTable requests
    tryCreateTable properties
    tryCreateTable comments
    tryCreateTable anns
    tryCreateTable apiKeys
    tryCreateTable securityTokens
    tryCreateTable member


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
          Single #email :- index
        ]
        $ toName "user"


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
        [ (#requestId :+ #name) :- primary,
          #authorId :- foreignKey users #_id,
          #requestId :- foreignKey requests #_id,
          (#requestId :+ #name :+ #dateAdded) :- index
        ]
        $ toName "property"


comments :: Table Comment
comments =
    tableFieldMod
        "comments"
        [ #_id :- autoPrimary,
          #authorId :- foreignKey users #_id,
          #requestId :- foreignKey requests #_id,
          (#_id :+ #requestId :+ #dateAdded) :- index
        ]
        $ toName "comment"


anns :: Table Ann
anns =
    tableFieldMod
        "announcements"
        [ #_id :- autoPrimary,
          #authorId :- foreignKey users #_id,
          (#active :+ #dateCreated) :- index
        ]
        $ toName "ann"


apiKeys :: Table ApiKey
apiKeys =
    tableFieldMod
        "api_keys"
        [ #key :- primary,
          #userId :- foreignKey users #_id,
          Single #dateCreated :- index,
          (#userId :+ #key) :- index
        ]
        $ toName "api_key"


securityTokens :: Table SecurityToken
securityTokens =
    tableFieldMod
        "security_tokens"
        [ #token :- primary,
          (#email :+ #token) :- index
        ]
        $ toName "security_token"


member :: Table Member
member =
    tableFieldMod
        "member"
        [ (#userId :+ #teamId) :- primary,
          #userId :- foreignKey users #_id,
          #teamId :- foreignKey teams #_id,
          (#userId :+ #teamId) :- index
        ]
        $ toName "member"


toName :: Text -> Text -> Text
toName name col
    | T.isPrefixOf "_" col = T.append name col
    | T.isPrefixOf name col =
        T.drop 1
            . camelToSnake
            . fromMaybe col
            $ T.stripPrefix name col
    | otherwise = camelToSnake col


camelToSnake :: Text -> Text
camelToSnake t = T.pack (T.unpack t >>= (\x -> if isUpper x then ['_', toLower x] else [x]))
