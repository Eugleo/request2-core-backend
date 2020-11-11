module Api.Query.Parser (parseQuerySpec) where

import Api.Query
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


parseQuerySpec :: Text -> Either Text QuerySpecification
parseQuerySpec txt = case runParser querySpec "" txt of
    Left err -> Left . pack $ errorBundlePretty err
    Right q -> Right q


type Parser = Parsec Void Text


querySpec :: Parser QuerySpecification
querySpec = Conjunction <$ space <*> many (lexeme clause) <* eof


clause :: Parser Clause
clause = try qualifiedClause <|> literalClause


literalClause :: Parser Clause
literalClause = try negated <|> extant
  where
    negated = Negated <$ lexeme (string "NOT") <*> literal
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


text :: Parser Text
text = pack <$> (quotedText <|> some (alphaNumChar <|> char '-'))
  where
    quotedText = char '\"' *> manyTill L.charLiteral (char '\"')


listOf :: Parser a -> Parser [a]
listOf = listOf' ','


listOf' :: Char -> Parser a -> Parser [a]
listOf' c p = (:) <$> p <*> many (lexeme (char c) *> p)


lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 empty empty)
