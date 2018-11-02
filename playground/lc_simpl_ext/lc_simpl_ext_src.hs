{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.Types (Constraint)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Pos
import Text.Parsec.ByteString
import Data.Functor
import Data.Char

-- Simply-typed lambda calculus with integer literals
-- and explicit type annotatopns.
-- Definition from Trees That Grow arcticle.


type family XLit idx
type family XVar idx
type family XAnn idx
type family XLam idx
type family XApp idx
type family XLet idx
type family XExp idx

-- Variable name: x, y in Variables
type Var = String

-- Type: Int | A -> B
data Typ = Int
         | Fun Typ Typ
        deriving Show

data ExpX idx where
    LitX :: XLit idx -> Integer  -> ExpX idx
    VarX :: XVar idx -> Var      -> ExpX idx
    LamX :: XLam idx -> Var      -> ExpX idx -> ExpX idx
    AppX :: XApp idx -> ExpX idx -> ExpX idx -> ExpX idx
    AnnX :: XAnn idx -> ExpX idx -> Typ      -> ExpX idx
    ExpX :: XExp idx -> ExpX idx
    LetX :: XLet idx -> Var -> ExpX idx -> ExpX idx

type ForallX (phi :: * -> Constraint) idx = (phi (XLit idx),
                                             phi (XVar idx),
                                             phi (XAnn idx),
                                             phi (XLam idx),
                                             phi (XApp idx),
                                             phi (XLet idx),
                                             phi (XExp idx))

deriving instance ForallX Show idx => Show (ExpX idx)


-- Decorate constructores of ExpX with SourcePos field.

data SrcPos
type Exp = ExpX SrcPos

type instance XLit SrcPos = SourcePos
type instance XVar SrcPos = SourcePos
type instance XLam SrcPos = SourcePos
type instance XApp SrcPos = SourcePos
type instance XAnn SrcPos = SourcePos
type instance XLet SrcPos = SourcePos
type instance XExp SrcPos = SourcePos

pattern Lit :: SourcePos -> Integer -> Exp
pattern Lit p i = LitX p i

pattern Var :: SourcePos -> Var -> Exp
pattern Var p v = VarX p v

pattern Lam :: SourcePos -> Var -> Exp -> Exp
pattern Lam p v e = LamX p v e

pattern App :: SourcePos -> Exp -> Exp -> Exp
pattern App p e1 e2 = AppX p e1 e2

pattern Ann :: SourcePos -> Exp -> Typ -> Exp
pattern Ann p e t = AnnX p e t

pattern Let :: SourcePos -> Var -> Exp -> Exp
pattern Let p v e = LetX p v e

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

lit :: Stream s m Char => ParsecT s u m Exp
lit = Lit <$> getPosition <*> (read <$> betweenSpaces (many1 digit))

-- Variable name

varRow :: Stream s m Char => ParsecT s u m String
varRow = betweenSpaces (many1 letter)

var :: Stream s m Char => ParsecT s u m Exp
var = Var <$> getPosition <*> varRow

-- Lambda

lamBound :: Stream s m Char => ParsecT s u m [Var]
lamBound = char '#' *> betweenSpaces (many1 varRow) <* char '.'

lam :: Stream s m Char => ParsecT s u m Exp
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

ann :: Stream s m Char => ParsecT s u m Exp
ann = Ann <$> getPosition <*> app <*> typ

-- Application

term :: Stream s m Char => ParsecT s u m Exp
term = betweenSpaces (lit <|> var <|> lam <|> betweenParan app)

app :: Stream s m Char => ParsecT s u m Exp
app = foldl <$> App <$> getPosition <*> term <*> many term

-- Binding

bindLet :: Stream s m Char => ParsecT s u m Var
bindLet = string "let" *> betweenSpaces varRow <* char '='

bind :: Stream s m Char => ParsecT s u m Exp
bind = Let <$> getPosition <*> bindLet <*> app

--

expression :: Stream s m Char => ParsecT s u m Exp
expression = bind <* eof

-- Parser
parseExp :: String -> Either ParseError Exp
parseExp = parse expression ""

---

eol :: Stream s m Char => ParsecT s u m ()
eol = void newline <|> eof

-- Parse source code --

parseCode :: Stream s m Char => ParsecT s u m [Exp]
parseCode = endBy (bind <* (char ';')) eol

parseFile = parseFromFile parseCode
