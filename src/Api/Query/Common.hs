{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Query.Common where

import Api.Query (Delimited (..), Entity)
import Data.Bifunctor (Bifunctor (second))
import Data.Char (isLower)
import Data.Maybe (mapMaybe)
import Data.Model.DateTime (DateTime, parseDateTime)
import Data.Model.Role (Role (..))
import Data.Model.Status (Status (..))
import Data.Text (toLower, unpack)
import Database.Selda hiding (second)
import Database.Selda.Unsafe (operator)
import Text.Read (readMaybe)


type QueryBuilder t a = Row t a -> Query t ()


type Translator t a b = a -> Either Text (QueryBuilder t b)


type EntityTranslator t a = (Col t Bool -> Col t Bool) -> Translator t Entity a


type Modifier t = Col t Bool -> Col t Bool


data GenericSelector t where
    ToSelector :: (SqlType a) => Selector t a -> GenericSelector t


makeSimpleSorter :: [(Text, GenericSelector a)] -> Translator t Text a
makeSimpleSorter = makeSorter . map (second orderBy)


orderBy :: GenericSelector a -> Order -> QueryBuilder t a
orderBy (ToSelector s) o a = order (a ! s) o


makeSorter :: [(Text, Order -> QueryBuilder t a)] -> Translator t Text a
makeSorter fields field = toEither . lookup field $ addAscDesc fields
  where
    toEither (Just x) = Right x
    toEither Nothing = Left $ "Sorting by " <> field <> " isn't supported"
    triplet n q =
        [ (n, q ascending),
          (n <> "-asc", q ascending),
          (n <> "-desc", q descending)
        ]
    addAscDesc = foldr (\(n, q) acc -> triplet n q ++ acc) []


fromEqual :: Text -> Delimited a -> Either Text a
fromEqual _ (Equal x) = Right x
fromEqual nm _ =
    Left $
        "The quantifier "
            <> nm
            <> " doesn't support any ordering operators"


approx :: Text -> Text
approx txt = "%" <> txt <> "%"


multiple :: (Col a1 Bool -> Col t Bool) -> (a2 -> Col a1 Bool) -> [a2] -> Query t ()
multiple f p = restrict . f . disj . map p


delimited :: SqlOrd a => (Col t Bool -> Col t Bool) -> Col t a -> [Delimited a] -> Query t ()
delimited f = multiple f . singleDelimited


singleDelimited :: (SqlOrd a) => Col t a -> Delimited a -> Col t Bool
singleDelimited field (LessThan val) = field .< literal val
singleDelimited field (GreaterThan val) = field .> literal val
singleDelimited field (LessOrEq val) = field .<= literal val
singleDelimited field (GreaterOrEq val) = field .>= literal val
singleDelimited field (Between l r) = field .> literal l .&& field .< literal r
singleDelimited field (Equal val) = field .== literal val


similar :: Modifier t -> Col t Text -> [Text] -> Query t ()
similar f = multiple f . singleSimilar


-- TODO Change t to PG to signal the use of ILIKE?
singleSimilar :: Col t Text -> Text -> Col t Bool
singleSimilar field val = field `op` literal (approx val)
  where
    op = if all isLower (unpack val) then operator "ILIKE" else like


exact :: SqlType a => Modifier t -> Col t a -> [a] -> Query t ()
exact f = multiple f . singleExact


singleExact :: SqlType a => Col t a -> a -> Col t Bool
singleExact field val = field .== literal val


parseDate :: Traversable t => t Text -> Maybe (t DateTime)
parseDate = traverse parseDateTime


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


parseType :: Text -> Maybe Text
parseType t
    | toLower t `elem` ["p", "proteomics"] = Just "proteomics"
    | toLower t `elem` ["s", "molecules", "small molecules"] = Just "small-molecules"
    | toLower t `elem` ["l", "lipidomics"] = Just "lipidomics"
    | toLower t `elem` ["m", "imaging", "ms imaging", "mass spectrometry imaging"] =
        Just "ms-imaging"
    | toLower t `elem` ["g", "analytics", "general analytics"] = Just "general-analytics"
    | otherwise = Nothing


parseStatus :: Text -> Maybe Status
parseStatus t
    | toLower t == "pending" = Just Pending
    | toLower t == "in progress" = Just InProgress
    | toLower t == "done" = Just Done
    | toLower t == "awaiting input" = Just AwaitingInput
    | toLower t == "deleted" = Just Deleted
    | otherwise = Nothing


disj :: Foldable f => f (Col a Bool) -> Col a Bool
disj = foldr (.||) false


idQualifier ::
    (HasField "_id" a, FieldType "_id" a ~ ID a) =>
    Modifier t ->
    Translator t [Delimited Text] a
idQualifier f = return . (\vs t -> delimited f (t ! #_id) vs) . mapMaybe parseId


activeQualifier ::
    (HasField "active" a, FieldType "active" a ~ Bool) =>
    Modifier t ->
    Translator t [Delimited Text] a
activeQualifier f vals = do
    vs <- mapM (fromEqual "active") vals
    return $ \t -> exact f (t ! #active) $ mapMaybe parseBool vs


undefinedQualifier :: Text -> Text -> Either Text a
undefinedQualifier name section =
    Left $ "The quantifier " <> name <> " is not defined for " <> section


literalName ::
    (Monad m, HasField "name" a, FieldType "name" a ~ Text) =>
    Modifier t ->
    Text ->
    m (Row t a -> Query t ())
literalName f txt = return $ \u -> restrict . f $ singleSimilar (u ! #name) txt
