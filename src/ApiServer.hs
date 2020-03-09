{-# LANGUAGE OverloadedStrings #-}

module ApiServer where

import qualified Auth
import Capability
import Config
import Data.Text.Lazy
import Web.Scotty

apiServer :: ServerConfig -> IO ()
apiServer config =
  scotty (listenPort config) $
    {-
     - this is slightly better than publishing an exact version
     -}
    do
      get "/capability" $ json (capabilityList :: [Text])
      {-
       - login/logout/auth functions
       -}
      post "/login" $ Auth.login config --get auth token
      post "/logout" $ Auth.logout config
      post "/password" $ auth Auth.changePassword
      {-
       - example authentized API (TODO remove)
       -}
      get (capture "/news") $ auth $ \_ _ -> text "hello!"
      {-
       - standard 404 -- keep this last
       -}
      notFound $ text "Not found"
  where
    auth x = Auth.authentized config (x config)
