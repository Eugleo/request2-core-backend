module Api.Query.Parser where

import Api.Query
import Data.Maybe (fromJust)
import Data.Model.DateTime
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint)

prettyParseQuery :: Text -> IO ()
prettyParseQuery = pPrint . fromJust . parseMaybe query

parseQuery :: Text -> IO ()
parseQuery = parseTest query

type Parser = Parsec Void Text

query :: Parser Query
query = Conjunction <$> listOf' ' ' clause <* eof

clause :: Parser Clause
clause = (try qualifiedClause) <|> literalClause

literalClause :: Parser Clause
literalClause = try negated <|> extant
  where
    negated = Negated <$ (lexeme $ string "NOT") <*> literal
    extant = Extant <$> literal

qualifiedClause :: Parser Clause
qualifiedClause = try negated <|> extant
  where
    negated = Negated <$ char '-' <*> qualifiedEntity
    extant = Extant <$> qualifiedEntity

literal :: Parser Entity
literal = Literal <$> text

qualifiedEntity :: Parser Entity
qualifiedEntity = do
  name <- pack <$> some letterChar
  char ':'
  qualified name

qualified :: Text -> Parser Entity
qualified name = Qualified name <$> listOf (delimited text)

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
text = pack <$> (quotedText <|> some alphaNumChar)
  where
    quotedText = between (char '"') (char '"') (many word)
    word = spaceChar <|> alphaNumChar

listOf :: Parser a -> Parser [a]
listOf = listOf' ','

listOf' :: Char -> Parser a -> Parser [a]
listOf' c p = (:) <$> p <*> many (char c *> p)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (char ' ' *> pure ())