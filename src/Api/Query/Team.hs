{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Api.Query.Team where

import Api.Query hiding (Query)
import qualified Api.Query as Api (Query)
import Control.Monad (forM_)
import Data.Environment (EnvAction, envIO)
import Data.Maybe
import Data.Model.Team
import Data.Text (append, toLower)
import Database.Selda
import Database.Selda.PostgreSQL (PG)
import Database.Table
import Text.Pretty.Simple (pPrint)

buildTeamQuery :: Api.Query -> EnvAction [Team]
buildTeamQuery q = do
  envIO $ pPrint q
  query $ do
    team <- select teams
    buildTeamPredicate q team
    return team

-- Let's hope Selda translates restrict p1 >>= restrict p2 into WHERE p1 AND p2
buildTeamPredicate :: Api.Query -> Row PG Team -> Query PG ()
buildTeamPredicate (Conjunction cs) t = mapM_ (queryFromClause t) cs

queryFromClause :: Row PG Team -> Clause -> Query PG ()
queryFromClause t (Extant ent) = queryFromEntity t ent id
queryFromClause t (Negated ent) = queryFromEntity t ent not_

queryFromEntity :: Row PG Team -> Entity -> (Col PG Bool -> Col PG Bool) -> Query PG ()
queryFromEntity t ent f =
  case ent of
    Literal txt -> restrict . f $ t ! #name `like` literal (approx txt)
    QualifiedText "member" vals -> forM_ vals $ \v -> do
      user <- select users
      restrict $ user ! #name `like` literal (approx v)
      restrict $ t ! #_id .== user ! #teamId
    QualifiedText "name" vals -> rfd $ map (\v -> t ! #name `like` literal (approx v)) vals
    QualifiedText "code" vals -> rfd $ map (\v -> t ! #code `like` literal (approx v)) vals
    QualifiedText "active" vals ->
      rfd $
        map (\v -> t ! #active .== literal v) $
          mapMaybe maybeParseBool vals
    QualifiedNumber "id" vals -> rfd $ map (delimitedPredicate (t ! #_id)) $ map (toId <$>) vals
  where
    rfd = restrict . f . disj

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