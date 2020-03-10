{-# LANGUAGE OverloadedStrings #-}

module ApiServer where

import qualified API.Announcement as Ann
import qualified Auth
import Capability
import Config
import Data.Text.Lazy
import Model.User (Role (..))
import Web.Scotty

apiServer :: ServerConfig -> IO ()
apiServer config =
  scotty (listenPort config) $
    do
      {-
       - Capabilities
       -}
      get "/capability" $ json (capabilityList :: [Text])
      {-
       - Users
       -}
      post "/login" $ Auth.login config --get auth token
      post "/logout" $ Auth.logout config
      post "/password" $ auth Auth.changePassword
      {-
       - Announcements
       -}
      post "/announcements" $ auth $ privileges [Operator] Ann.create
      get "/annoncements" $ auth $ privileges [Basic] Ann.getAll
      get "/announcement/:ann_id" $ auth $ privileges [Basic] Ann.get
      delete "/announcement/:ann_id" $ auth $ privileges [Admin] Ann.deactivate
      put "/announcement/:ann_id" $ auth $ privileges [Admin] Ann.edit
      {-
       - Standard 404 -- keep this last
       -}
      notFound $ text "Not found"
  where
    auth x = Auth.authentized config (x config)
    privileges roles f c = Auth.withPrivileges c roles $ f c
