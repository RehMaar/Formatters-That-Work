{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.Char
import Data.Functor

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
data Expr = Lit Integer  -- Literal: i
         | Var Var       -- Variable: x
         | Ann Expr Typ  -- Annotation: M :: A
         | Lam Var Expr  -- Lambda abstraction: \ x . N
         | App Expr Expr -- Application: L M
         | Let Var Expr  -- Bind name to Expr
        deriving Show

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

betweenSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenSpaces = between spaces spaces

betweenParan :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenParan = between (char '(') (char ')')

-- Literal

lit :: Stream s m Char => ParsecT s u m Expr
lit = Lit <$> read <$> betweenSpaces (many1 digit)

-- Variable name

varRow :: Stream s m Char => ParsecT s u m String
varRow = betweenSpaces (many1 letter)

var :: Stream s m Char => ParsecT s u m Expr
var = Var <$> varRow

-- Lambda

lamBound :: Stream s m Char => ParsecT s u m [Var]
lamBound = char '#' *> betweenSpaces (many1 varRow) <* char '.'

lam :: Stream s m Char => ParsecT s u m Expr
lam = flip (foldr Lam) <$> lamBound <*> app

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
ann = Ann <$> app <*> typ

-- Application

term :: Stream s m Char => ParsecT s u m Expr
term = betweenSpaces (lit <|> var <|> lam <|> betweenParan app)

app :: Stream s m Char => ParsecT s u m Expr
app = foldl App <$> term <*> many term

-- Binding

bindLet :: Stream s m Char => ParsecT s u m Var
bindLet = string "let" *> betweenSpaces varRow <* char '='

bind :: Stream s m Char => ParsecT s u m Expr
bind = Let <$> bindLet <*> app

-- Parser

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (bind <|> app <* eof) ""

----------------

main = do
    line <- getLine
    case parseExpr line of
        Left e     -> putStrLn "parse error"
        Right expr -> putStrLn $ show expr
