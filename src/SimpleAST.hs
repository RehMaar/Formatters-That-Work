{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleAST where
    
data PlaceHolder
data Import = Import {
    iName :: String,
    iQuilified :: Bool,
    iAs :: Maybe String
}

type Name = String

--data SigType = OtherST | TypeST deriving Show

data Sig = Type [Name] Type  deriving Show

data AppType = AppInfix Name
             | AppPrefix Type
             deriving Show

data Type = TyVar Name
          | TyApps [AppType]
          | TyApp Type Type
          | TyFun Type Type
          | TyTuple [Type]
          | TyList Type
         deriving Show


data ConstPatDetail = PrefixConPat [Pat]
                    | InfixConPat Pat Pat

data Pat = VarPat Name
             -- | ListPat
             -- | TuplePat
             | ParPat Pat
             | NPat OverLiterals
             | ConstPat Name ConstPatDetail
             | WildPat


{-
name m_args | m_stmts!!0 = exprs!!0
            | m_stmts!!1 = exprs!!1
            ...
            | m_stmts!!n = expts!!n
    where
      locals
-}
data Match = Match { m_args :: [Pat], m_stmts :: [[Stmt]], m_exprs :: [Expr], m_locals :: LocalBind } | Dontknow

newtype MatchGroup = MG [Match]

data LocalBind = ValLocalBind [Bind] [Sig]| EmptyLocalBind

data Bind = FunBind {
        fun_name :: String,
        fun_matches :: [Match]
    }
  | OtherBind


data Stmt = BindStmt Pat Expr -- TODO?
          | BodyStmt Expr     -- in guards?
          | LetStmt LocalBind -- TODO?

data Literals = LitChar Char | LitString String

data OverLiterals = OverLitInteger Integer
                  | OverLitFractional Rational
                  | OverLitString String

data Expr = Var Name
          | OverLit OverLiterals
          | Lit Literals
          | Lam Match
          | App Expr Expr -- (Match Expr) :@ (Match Expr)
          | OpApp Expr Expr Expr -- lexpr `op` rexpr
          | Let LocalBind Expr -- let bind = expr
          | If Expr Expr Expr -- pred then else
          | Do [Stmt]
          | Case Expr Match
          | ExprWithType Expr Type

data Decls = ValDecl Bind
           | SigDecl Sig

data Hs = Hs {
    hsModuleName :: Maybe String,
    hsImports :: [Import],
    hsDecls   :: [Decls]
}

deriving instance Show ConstPatDetail
deriving instance Show Stmt
deriving instance Show Pat
deriving instance Show Literals
deriving instance Show OverLiterals
deriving instance Show Expr
deriving instance Show LocalBind
deriving instance Show Match
deriving instance Show Bind
deriving instance Show Decls
deriving instance Show Import
deriving instance Show Hs
