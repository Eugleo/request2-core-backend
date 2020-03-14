{-# LANGUAGE OverloadedStrings #-}

module ApiServer where

import qualified API.Announcement as Ann
import qualified Auth
import qualified Data.Text.Lazy as T
import Capability
import Config
import Control.Monad
import Model.User (Role(..))
import UserInfo
import Network.Wai.Middleware.Cors
import Web.Scotty

apiServer :: ServerConfig -> IO ()
apiServer config =
  scotty (listenPort config) $ do
    when (allowCORS config) $ middleware simpleCors
      {-
       - Capabilities
       -}
    get "/capability" $ json (capabilityList :: [T.Text])
      {-
       - Users
       -}
    post "/register" undefined
    post "/register-verify/:token" undefined
    post "/login" $ Auth.login config --get auth token
    post "/logout" $ Auth.logout config
    post "/password" $ auth Auth.changePassword
      {-
       - User information
       -}
    get "/userinfo" $ auth undefined --TODO get user info
    put "/userinfo" $ auth undefined --TODO get user info
      {-
       - Announcements
       -}
    get "/announcements" $ auth $ nui Ann.getAll
    get "/announcement/:ann_id" $ auth $ nui Ann.get
    post "/announcements" $ authAdmin $ nui Ann.create
    put "/announcement/:ann_id" $ authAdmin $ nui Ann.edit
    delete "/announcement/:ann_id" $ authAdmin $ nui Ann.deactivate
      {-
       - Standard 404 -- keep this last
       -}
    notFound $ text "Not found"
  where
    nui x conf _ = x conf --No User Info (this really calls for Reader...)
    auth x = Auth.authentized config (x config)
    satisfyRole cond x =
      auth $ \conf ui ->
        if any cond (roles ui)
          then x conf ui
          else Auth.finishForbidden
    authClient = satisfyRole (>= Client)
    authOper = satisfyRole (>= Operator)
    authAdmin = satisfyRole (>= Admin)
