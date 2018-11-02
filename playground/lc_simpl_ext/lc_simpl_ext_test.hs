{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
import GHC.Types (Constraint)

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
-- Old definition.
-- Expressions: i | x | M :: A | \ x . N | L M
data Exp = Lit Integer   -- Literal: i
         | Var Var       -- Variable: x
         | Ann Exp Typ   -- Annotation: M :: A
         | Abs Var Exp   -- Absbda abstraction: \ x . N
         | App Exp Exp   -- Application: L M
        deriving Show
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

-- Extend ExpX with new field for App for type checking.

data TC
type ExpTC = ExpX TC

type instance XLit TC = ()
type instance XVar TC = ()
type instance XAnn TC = ()
type instance XAbs TC = ()
type instance XApp TC = Typ
type instance XExp TC = ()

pattern AppTC :: Typ -> ExpTC -> ExpTC -> ExpTC
pattern AppTC t l m = AppX t l m

pattern LitTC :: Integer -> ExpTC
pattern LitTC i <- LitX _ i
    where LitTC i = LitX () i

pattern VarTC :: String -> ExpTC
pattern VarTC s <- VarX _ s
    where VarTC s = VarX () s

pattern AnnTC :: ExpTC -> Typ -> ExpTC
pattern AnnTC e t  <- AnnX _ e t
    where AnnTC e t = AnnX () e t

pattern AbsTC :: Var -> ExpTC -> ExpTC
pattern AbsTC e t  <- AbsX _ e t
    where AbsTC e t = AbsX () e t

-- Typing rules from the articles.

check :: ExpTC -> [(Var, Typ)] -> Typ -> Bool
check (LitTC _)     _   Int       = True
check (VarTC x)     ctx c         = maybe False (== c) (lookup x ctx)
check (AnnTC m a)   ctx c         = a == c && check m ctx c
check (AbsTC x n)   ctx (Fun a b) = check n ((x, a) : ctx) b
check (AppTC a l m) ctx c         = check l ctx (Fun a c) && check m ctx a
check _             _   _         = False

-- Extend ExpX with new constructor

data Val = Val String

data PE

type ExpPE = ExpX PE

type instance XLit PE = ()
type instance XVar PE = ()
type instance XAnn PE = ()
type instance XAbs PE = ()
type instance XApp PE = ()
type instance XExp PE = Val

pattern AppPE :: ExpPE -> ExpPE -> ExpPE
pattern AppPE l m <- AppX _ l m

pattern LitPE :: Integer -> ExpPE
pattern LitPE i <- LitX _ i
    where LitPE i = LitX () i

pattern VarPE :: String -> ExpPE
pattern VarPE s <- VarX _ s
    where VarPE s = VarX () s

pattern AnnPE :: ExpPE -> Typ -> ExpPE
pattern AnnPE e t  <- AnnX _ e t
    where AnnPE e t = AnnX () e t

pattern AbsPE :: Var -> ExpPE -> ExpPE
pattern AbsPE e t  <- AbsX _ e t
    where AbsPE e t = AbsX () e t

pattern ValPE :: Val -> ExpPE
pattern ValPE v = ExpX v

-- Functions for extensible data types.
-- Printing our data types.

printT :: Typ -> String
printT Int = "Int"
printT (Fun a b) = "(" ++ printT a ++ ")" ++ " -> " ++ printT b

printE :: (XExp idx -> String) -> ExpX idx -> String
printE _ (LitX _ i) = show i
printE _ (VarX _ x) = x
printE p (AnnX _ m a) = "(" ++ printE p m ++ ") :: (" ++ printT a ++ ")"
printE p (AbsX _ x n) = "# " ++ x ++ " . " ++ printE p n
printE p (AppX _ l m) = "(" ++ printE p l ++ ") (" ++ printE p m ++ ")"
printE p (ExpX idx)   = p idx

printUD :: Exp -> String
printUD = printE undefined

printETC :: ExpTC -> String
printETC = printE undefined

printEPE :: ExpPE -> String
printEPE = printE p
    where p v = "{{" ++ show v ++ "}}"
deriving instance Show Val


-- TypeClasses for EDT.

--printE' :: (XLit idx -> String) -> (XVar idx -> String) ->
--           (XAnn idx -> String) -> (XAbs idx -> String) ->
--           (XApp idx -> String) -> (XExp idx -> String) ->
--           ExpX idx -> String
--printE' = undefined

type ForallX (phi :: * -> Constraint) idx = (phi (XLit idx),
                                             phi (XVar idx),
                                             phi (XAnn idx),
                                             phi (XAbs idx),
                                             phi (XApp idx),
                                             phi (XExp idx))

deriving instance ForallX Show idx => Show (ExpX idx)

-- Replacing constructors.
{-
data SA

type ExpSA = ExpX SA
type instance XLit SA = ()
type instance XVar SA = ()
type instance XAnn SA = ()
type instance XAbs SA = ()
type instance XApp SA = ()
type instance XExp SA = (ExpSA, [ExpSA])

pattern AppSA :: ExpSA -> [ExpSA] -> ExpSA
pattern AppSA l ms = ExpX (l, ms)
-}
-- TODO: use it.

-- Extension using type parameters.

type family XPLit idx a
type family XPVar idx a
type family XPAnn idx a
type family XPAbs idx a
type family XPApp idx a
type family XPExp idx a

-- ExpX has extra constructor ExpX. This field is used to extend the
-- data type with new constructor.
data ExpXP idx a = LitXP (XPLit idx a) Integer
                 | VarXP (XPVar idx a) Var
                 | AnnXP (XPAnn idx a) (ExpXP idx a) Typ
                 | AbsXP (XPAbs idx a) Var (ExpXP idx a)
                 | AppXP (XPApp idx a) (ExpXP idx a) (ExpXP idx a)
                 | ExpXP (XPExp idx a)

-- Add Let extension.
-- let expr in expr ?
data LE
type ExpLE a = ExpXP LE a

type instance XPLit LE a = ()
type instance XPVar LE a = ()
type instance XPAnn LE a = ()
type instance XPAbs LE a = ()
type instance XPApp LE a = ()
type instance XPExp LE a = (a, ExpLE a, ExpLE a)

pattern LetLE :: a -> ExpLE a -> ExpLE a -> ExpLE a
pattern LetLE x m n = ExpXP (x, m, n)

-- TODO: use it.

-- GADTs

type family NXLit idx c
type family NXApp idx a b
type family NXAdd idx
type family NXAnd idx
type family NXExp idx a

data ExpNX idx a where
    ConNX :: NXCon idx c   -> c -> ExpNX a
    AppNX :: NXApp idx a b -> ExpNX (a -> b) -> ExpNX a -> ExpNX b
    AddNX :: NXAdd idx     -> ExpNX (Int -> Int -> Int)
    AndNX :: NXAnd idx     -> ExpNX (Bool -> Bool -> Bool)
    ExpNX :: NXEcp idx a   -> ExpNX idx a

-- Extend App with new field to store a printer for a
data Pr
type ExpPr a = ExpNX Pr a

type instance NXCon Pr c   = ()
type instance NXApp Pr a b = (a -> String)
type instance NXAdd Pr     = ()
type instance NXAnd Pr     = ()
type instance NXExp Pr a   = ()

pattern AppPr :: (a -> String) -> ExpPr (a -> b) -> ExpPr a -> ExpPr b
pattern AppPr p l m = AppNX p l m
