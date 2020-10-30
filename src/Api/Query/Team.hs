{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Team where

import Api.Query hiding (Query)
import qualified Api.Query as Api (Query)
import Data.Environment (EnvAction, envIO)
import Data.Maybe
import Data.Model.Team
import Data.Text (append, toLower, unpack)
import Database.Selda
import Database.Selda.PostgreSQL (PG)
import Database.Table
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)

type FilteringQuery = Either Text (Row PG Team -> (Query PG ()))

buildTeamQuery :: Api.Query -> EnvAction (Either Text [Team])
buildTeamQuery q = do
  envIO $ pPrint q
  case buildTeamPredicate q of
    Right p -> do
      res <- query $ do
        team <- select teams
        p team
        return team
      return $ Right res
    Left message -> return $ Left message

buildTeamPredicate :: Api.Query -> FilteringQuery
buildTeamPredicate (Conjunction cs) =
  case mapM (queryFromClause) cs of
    Right preds -> Right $ \t -> mapM_ ($ t) preds
    Left message -> Left message

queryFromClause :: Clause -> FilteringQuery
queryFromClause (Extant ent) = queryFromEntity ent id
queryFromClause (Negated ent) = queryFromEntity ent not_

queryFromEntity :: Entity -> (Col PG Bool -> Col PG Bool) -> FilteringQuery
queryFromEntity ent f =
  case ent of
    Literal txt ->
      Right $ \t ->
        restrict . f $ t ! #name `like` literal (approx txt)
    Qualified "member" vals -> do
      vs <- mapM (fromEqual "member") vals
      return $ \t -> do
        user <- select users
        restrict . disj $ map (\v -> user ! #name `like` literal (approx v)) vs
        restrict $ t ! #_id .== user ! #teamId
    Qualified "name" vals -> do
      vs <- mapM (fromEqual "name") vals
      return $ \t ->
        rfd $
          map
            (\v -> t ! #name `like` literal (approx v))
            vs
    Qualified "code" vals -> do
      vs <- mapM (fromEqual "code") vals
      Right $ \t ->
        rfd $
          map
            (\v -> t ! #code `like` literal (approx v))
            vs
    Qualified "active" vals -> do
      vs <- mapM (fromEqual "active") vals
      Right $ \t ->
        rfd $
          map (\v -> t ! #active .== literal v) $
            mapMaybe maybeParseBool vs
    Qualified "id" vals ->
      Right $ \t ->
        rfd $
          map (delimitedPredicate (t ! #_id)) $
            mapMaybe parseId vals
    Qualified quant _ ->
      Left ("The quantifier " `append` quant `append` " is not defined for teams")
  where
    rfd = restrict . f . disj
    parseId = sequence . fmap (fmap toId . readMaybe . unpack)

-- TODO Error out in other cases
fromEqual :: Text -> Delimited a -> Either Text a
fromEqual _ (Equal x) = Right x
fromEqual nm _ =
  Left $
    "The quantifier "
      `append` nm
      `append` " doesn't support any ordering operators"

approx :: Text -> Text
approx txt = "%" `append` txt `append` "%"

delimitedPredicate :: (SqlOrd a) => Col PG a -> Delimited a -> Col PG Bool
delimitedPredicate field (LessThan val) = field .< literal val
delimitedPredicate field (GreaterThan val) = field .> literal val
delimitedPredicate field (LessOrEq val) = field .<= literal val
delimitedPredicate field (GreaterOrEq val) = field .>= literal val
delimitedPredicate field (Between l r) = field .> literal l .&& field .< literal r
delimitedPredicate field (Equal val) = field .== literal val

maybeParseBool :: Text -> Maybe Bool
maybeParseBool t
  | toLower t == "true" = Just True
  | toLower t == "false" = Just False
  | otherwise = Nothing

disj :: Foldable f => f (Col a Bool) -> Col a Bool
disj cols = foldr (.||) false cols