module Api.Query.Parser where

import Api.Query
import Data.Model.DateTime
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

query :: Parser Query
query = Conjunction <$> listOf' '+' clause

clause :: Parser Clause
clause = (try qualifiedClause) <|> literalClause

literalClause :: Parser Clause
literalClause = extant <|> negated
  where
    extant = Extant <$> literal
    negated = Negated <$> (try (string "NOT") *> literal)

qualifiedClause :: Parser Clause
qualifiedClause = extant <|> negated
  where
    extant = Extant <$> qualifiedEntity
    negated = Negated <$> (try (string "-") *> qualifiedEntity)

literal :: Parser Entity
literal = Literal <$> text

qualifiedEntity :: Parser Entity
qualifiedEntity = do
  name <- pack <$> some asciiChar
  char ':'
  qualifiedText name <|> qualifiedDate name <|> qualifiedNumber name

qualifiedText :: Text -> Parser Entity
qualifiedText name = QualifiedText name <$> listOf text

qualifiedDate :: Text -> Parser Entity
qualifiedDate name = QualifiedDate name <$> listOf (delimited date)

qualifiedNumber :: Text -> Parser Entity
qualifiedNumber name = QualifiedNumber name <$> listOf (delimited L.decimal)

delimited :: Parser a -> Parser (Delimited a)
delimited p = try gte <|> gt <|> try lte <|> lt <|> try range <|> eq
  where
    gt = GreaterThan <$ string ">" <*> p
    gte = GreaterOrEq <$ string ">=" <*> p
    lt = LessThan <$ string "<" <*> p
    lte = LessOrEq <$ string "<=" <*> p
    range = Between <$> p <* string ".." <*> p
    eq = Equal <$> p

date :: Parser DateTime
date = do
  year <- L.decimal
  char '-'
  month <- L.decimal
  char '-'
  day <- L.decimal
  return $ fromDate year month day

text :: Parser Text
text = pack <$> (quotedText <|> some letterChar)
  where
    quotedText = between (char '"') (char '"') (many word)
    word = spaceChar <|> letterChar

listOf :: Parser a -> Parser [a]
listOf = listOf' ','

listOf' :: Char -> Parser a -> Parser [a]
listOf' c p = flip (:) <$> many (p <* char c) <*> p

sc :: Parser ()
sc = L.space (char '+' *> pure ()) empty empty