{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Pos
import Text.Parsec.ByteString
import Data.Functor
import Data.Char

-- Simply-typed lambda calculus with integer literals
-- and explicit type annotatopns.
-- Definition from Trees That Grow arcticle.

-- Variable name: x, y in Variables
type Var = String

-- Type: Int | A -> B
data Typ = Int
         | Fun Typ Typ
        deriving Show

-- Expressions: i | x | M :: A | \ x . N | L M
data Expr = Lit SourcePos Integer  -- Literal: i
          | Var SourcePos Var       -- Variable: x
          | Lam SourcePos Var Expr  -- Lambda abstraction: \ x . N
          | Ann SourcePos Expr Typ  -- Annotation: M :: A
          | App SourcePos Expr Expr -- Application: L M
          | Let SourcePos Var Expr  -- Bind name to Expr
        deriving Show

data SourceCode = SC [Expr]

-- Parser --
{-
    Examples:
    * Literals: 4
    * Variable: x
    * Annotation: #x . x :: a
    * Abstraction: #x . x
    * Application: (#x. x) 2
    * Binding: let name = expr
-}


whitespace :: Stream s m Char => ParsecT s u m Char
whitespace = satisfy (\x -> isSpace x && x /= '\n')

whitespaces :: Stream s m Char => ParsecT s u m ()
whitespaces = skipMany whitespace

betweenSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenSpaces = between whitespaces whitespaces

betweenParan :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenParan = between (char '(') (char ')')

-- Literal

lit :: Stream s m Char => ParsecT s u m Expr
lit = Lit <$> getPosition <*> (read <$> betweenSpaces (many1 digit))

-- Variable name

varRow :: Stream s m Char => ParsecT s u m String
varRow = betweenSpaces (many1 letter)

var :: Stream s m Char => ParsecT s u m Expr
var = Var <$> getPosition <*> varRow

-- Lambda

lamBound :: Stream s m Char => ParsecT s u m [Var]
lamBound = char '#' *> betweenSpaces (many1 varRow) <* char '.'

lam :: Stream s m Char => ParsecT s u m Expr
lam = flip <$> foldr <$> Lam <$> getPosition <*> lamBound <*> app

-- Type annotation

typSep :: Stream s m Char => ParsecT s u m String
typSep = string "::"

typBase :: Stream s m Char => ParsecT s u m Typ
typBase = const Int <$> string "Int"

typFun :: Stream s m Char => ParsecT s u m Typ
typFun = foldl Fun <$> typBase <*> many1 (betweenSpaces (string "->" *> betweenSpaces (typBase <|> typFun)))

typ :: Stream s m Char => ParsecT s u m Typ
typ = typSep *> betweenSpaces (typFun <|> typBase)

ann :: Stream s m Char => ParsecT s u m Expr
ann = Ann <$> getPosition <*> app <*> typ

-- Application

term :: Stream s m Char => ParsecT s u m Expr
term = betweenSpaces (lit <|> var <|> lam <|> betweenParan app)

app :: Stream s m Char => ParsecT s u m Expr
app = foldl <$> App <$> getPosition <*> term <*> many term

-- Binding

bindLet :: Stream s m Char => ParsecT s u m Var
bindLet = string "let" *> betweenSpaces varRow <* char '='

bind :: Stream s m Char => ParsecT s u m Expr
bind = Let <$> getPosition <*> bindLet <*> app

--

expression :: Stream s m Char => ParsecT s u m Expr
expression = bind <* eof

-- Parser
parseExpr :: String -> Either ParseError Expr
parseExpr = parse expression ""

---

eol :: Stream s m Char => ParsecT s u m ()
eol = void newline <|> eof

bind' :: Stream s m Char => ParsecT s u m Expr
bind' = Let <$> getPosition <*> bindLet <*> var

-- Parse source code --

parseCode :: Stream s m Char => ParsecT s u m [Expr]
parseCode = endBy (bind <* (char ';')) eol

parseFile = parseFromFile parseCode
