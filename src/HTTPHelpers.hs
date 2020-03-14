module HTTPHelpers where

{- Note: these are low-level helpers. If you are working with EnvAction, use
 - envForbidden etc. instead. -}
import Network.HTTP.Types.Status
import Web.Scotty

finishForbidden :: ActionM a
finishForbidden = status forbidden403 >> finish

finishBadRequest :: ActionM a
finishBadRequest = status badRequest400 >> finish

finishNotFound :: ActionM a
finishNotFound = status notFound404 >> finish

finishServerError :: ActionM a
finishServerError = status internalServerError500 >> finish
