{-# LANGUAGE FlexibleContexts #-}

module Api.Query.Common where

import Api.Query hiding (Query)
import qualified Api.Query as Api (Query)
import Data.Environment
import Data.Model.DateTime
import Data.Model.Role
import Data.Text (append, toLower, unpack)
import Database.Selda
import Database.Selda.PostgreSQL (PG)
import Text.Read (readMaybe)

type QueryFor a = Either Text (Row PG a -> Query PG ())

buildQuery ::
  (Relational a) =>
  (Api.Query -> QueryFor a) ->
  Table a ->
  Api.Query ->
  EnvAction (Either Text [a])
buildQuery build tbl q =
  case build q of
    Right p -> do
      res <- query $ do
        item <- select tbl
        p item
        return item
      return $ Right res
    Left message -> return $ Left message

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