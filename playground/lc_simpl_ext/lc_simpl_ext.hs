{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving     #-}
{-# LANGUAGE TypeOperators, PatternSynonyms           #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts      #-}
--{-# LANGUAGE UndecidableInstances                     #-}
--import GHC.Types (Constraint)

import Text.Parsec
import Text.Parsec.Char
import Data.Functor

-- Simply-typed lambda calculus with integer literals
-- and explicit type annotatopns.
-- Definition from Trees That Grow article.

type Var = String

-- Type: Int | A -> B
data Typ = Int
         | Fun Typ Typ
        deriving (Eq, Show)

--    idx -- a type index to ExpX; extension descriptor.
--    XC -- type family; use to extend a data constructor with extra fields.
type family XLit idx
type family XVar idx
type family XAnn idx
type family XAbs idx
type family XApp idx
type family XExp idx

-- ExpX has extra constructor ExpX. This field is used to extend the
-- data type with new constructor.
data ExpX idx  = LitX (XLit idx) Integer
                | VarX (XVar idx) Var
                | AnnX (XAnn idx) (ExpX idx) Typ
                | AbsX (XAbs idx) Var (ExpX idx)
                | AppX (XApp idx) (ExpX idx) (ExpX idx)
                | ExpX (XExp idx)
{-
type ForallX (phi :: * -> Constraint) idx = (phi (XLit idx),
                                             phi (XVar idx),
                                             phi (XAnn idx),
                                             phi (XAbs idx),
                                             phi (XApp idx),
                                             phi (XExp idx))

deriving instance ForallX Show idx => Show (ExpX idx)
-}
-- Undecorated variant of ExpX.
data UD
type Exp = ExpX UD
type instance XLit UD = ()
type instance XVar UD = ()
type instance XAnn UD = ()
type instance XAbs UD = ()
type instance XApp UD = ()
type instance XExp UD = ()

incLitX :: Exp -> Exp
incLitX (LitX _ i) = LitX () (i + 1)
incLitX  e = e

-- Define for undecorated version of ExpX.
-- The same as old Exp
pattern Lit :: Integer -> Exp
pattern Lit i <- LitX _ i
    where Lit i = LitX () i

pattern Var :: String -> Exp
pattern Var s <- VarX _ s
    where Var s = VarX () s

pattern Ann :: Exp -> Typ -> Exp
pattern Ann e t  <- AnnX _ e t
    where Ann e t = AnnX () e t

pattern Abs :: Var -> Exp -> Exp
pattern Abs e t  <- AbsX _ e t
    where Abs e t = AbsX () e t

pattern App :: Exp -> Exp -> Exp
pattern App e t  <- AppX _ e t
    where App e t = AppX () e t

-- Parser --
{-
    Examples:
    * Literals: 4
    * Variable: x
    * Annotation: #x . x :: a
    * Abstraction: #x . x
    * Application: (#x. x) 2
-}
betweenSpaces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenSpaces = between spaces spaces

betweenParan :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenParan = between (char '(') (char ')')

lit :: Stream s m Char => ParsecT s u m Exp
lit = Lit <$> read <$> betweenSpaces (many1 digit)

varRow :: Stream s m Char => ParsecT s u m String
varRow = betweenSpaces (many1 letter)

var :: Stream s m Char => ParsecT s u m Exp
var = Var <$> varRow

term :: Stream s m Char => ParsecT s u m Exp
term = betweenSpaces (lit <|> var <|> lam <|> betweenParan app)

app :: Stream s m Char => ParsecT s u m Exp
app = foldl App <$> term <*> many term

lamBound :: Stream s m Char => ParsecT s u m [Var]
lamBound = char '#' *> betweenSpaces (many1 varRow) <* char '.'

lam :: Stream s m Char => ParsecT s u m Exp
lam = flip (foldr Abs) <$> lamBound <*> app

typSep :: Stream s m Char => ParsecT s u m String
typSep = string "::"

typBase :: Stream s m Char => ParsecT s u m Typ
typBase = const Int <$> string "Int"

typFun :: Stream s m Char => ParsecT s u m Typ
typFun = foldl Fun <$> typBase <*> many1 (betweenSpaces (string "->" *> betweenSpaces (typBase <|> typFun)))

typ :: Stream s m Char => ParsecT s u m Typ
typ = typSep *> betweenSpaces (typFun <|> typBase)

ann :: Stream s m Char => ParsecT s u m Exp
ann = Ann <$> app <*> typ

parseExp :: String -> Either ParseError Exp
parseExp = parse (app <* eof) ""

right (Right b) = b
expr1 = right $ parseExp "(# x . (# y . plus y x)) 12 14"
