{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Query.Common where

import Api.Query (Delimited (..), Entity)
import Data.Maybe (mapMaybe)
import Data.Model.DateTime
import Data.Model.Role
import Data.Text (append, toLower, unpack)
import Database.Selda
import Database.Selda.PostgreSQL (PG)
import Text.Read (readMaybe)

type QueryBuilder a = Row PG a -> Query PG ()

type Translator a b = a -> Either Text (QueryBuilder b)

type EntityTranslator a = (Col PG Bool -> Col PG Bool) -> Translator Entity a

fromEqual :: Text -> Delimited a -> Either Text a
fromEqual _ (Equal x) = Right x
fromEqual nm _ =
  Left $
    "The quantifier "
      `append` nm
      `append` " doesn't support any ordering operators"

approx :: Text -> Text
approx txt = "%" `append` txt `append` "%"

multiple :: (Col a1 Bool -> Col t Bool) -> (a2 -> Col a1 Bool) -> [a2] -> Query t ()
multiple f p = restrict . f . disj . map p

delimited :: SqlOrd a => (Col PG Bool -> Col t Bool) -> Col PG a -> [Delimited a] -> Query t ()
delimited f = multiple f . singleDelimited

singleDelimited :: (SqlOrd a) => Col PG a -> Delimited a -> Col PG Bool
singleDelimited field (LessThan val) = field .< literal val
singleDelimited field (GreaterThan val) = field .> literal val
singleDelimited field (LessOrEq val) = field .<= literal val
singleDelimited field (GreaterOrEq val) = field .>= literal val
singleDelimited field (Between l r) = field .> literal l .&& field .< literal r
singleDelimited field (Equal val) = field .== literal val

similar :: (Col PG Bool -> Col t Bool) -> Col PG Text -> [Text] -> Query t ()
similar f = multiple f . singleSimilar

singleSimilar :: Col PG Text -> Text -> Col PG Bool
singleSimilar field val = field `like` literal (approx val)

exact :: SqlType a => (Col PG Bool -> Col t Bool) -> Col PG a -> [a] -> Query t ()
exact f = multiple f . singleExact

singleExact :: SqlType a => Col PG a -> a -> Col PG Bool
singleExact field val = field .== literal val

-- TODO implement
parseDate :: Traversable t => t Text -> Maybe (t DateTime)
parseDate _ = Nothing

parseBool :: Text -> Maybe Bool
parseBool t
  | toLower t == "true" = Just True
  | toLower t == "false" = Just False
  | otherwise = Nothing

parseRole :: Text -> Maybe Role
parseRole t
  | toLower t == "admin" = Just Admin
  | toLower t == "client" = Just Client
  | toLower t == "operator" = Just Operator
  | otherwise = Nothing

parseId :: Traversable t => t Text -> Maybe (t (ID a))
parseId = traverse (fmap toId . readMaybe . unpack)

disj :: Foldable f => f (Col a Bool) -> Col a Bool
disj = foldr (.||) false

idQuantifier ::
  (HasField "_id" a, FieldType "_id" a ~ ID a) =>
  (Col PG Bool -> Col PG Bool) ->
  Translator [Delimited Text] a
idQuantifier f = return . (\vs t -> delimited f (t ! #_id) vs) . mapMaybe parseId

activeQuantifier ::
  (HasField "active" a, FieldType "active" a ~ Bool) =>
  (Col PG Bool -> Col PG Bool) ->
  Translator [Delimited Text] a
activeQuantifier f vals = do
  vs <- mapM (fromEqual "active") vals
  return $ \t -> exact f (t ! #active) $ mapMaybe parseBool vs

undefinedQuantifier :: Text -> Text -> Either Text a
undefinedQuantifier name section =
  Left $ "The quantifier " `append` name `append` " is not defined for " `append` section

literalName ::
  (Monad m, HasField "name" a, FieldType "name" a ~ Text) =>
  (Col PG Bool -> Col PG Bool) ->
  Text ->
  m (Row PG a -> Query PG ())
literalName f txt = return $ \u -> restrict . f $ singleSimilar (u ! #name) txt
