{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module TtgAST where


    
data PlaceHolder

{-
data Import = Import {
    iName :: String,
    iQuilified :: Bool,
    iAs :: Maybe String
}
-}

type family XImport idx
type family XImportCtr idx

data ImportX idx
  = ImportX {
    iExt :: XImportCtr idx,
    iName :: String,
    iQuilified :: Bool,
    iAs :: Maybe String }
  | ImportX' (XImport idx)

type Name = String

{-
data Sig idx = Type [Name] Type deriving Show
-}

type family XSig idx
type family XTypeSig idx

data SigX idx = Type (XTypeSig idx) [Name] (TypeX idx) | SigX (XSig idx)

{-
data AppType = AppInfix Name
             | AppPrefix Type
             deriving Show
 -}

type family XAppType idx
type family XAppPrefix idx
type family XAppIndix idx

data AppTypeX idx
  = AppInfixX (XAppIndix idx) Name
  | AppPrefixX (XAppPrefix idx) (TypeX idx)
  | AppTypeX (XAppType idx)

{-
data Type = TyVar Name
          | TyApps [AppType]
          | TyApp Type Type
          | TyFun Type Type
          | TyTuple [Type]
          | TyList Type
         deriving Show
-}

type family XType idx
type family XTyVar idx
type family XTyApps idx
type family XTyApp idx
type family XTyFun idx
type family XTyTuple idx
type family XTyList idx

data TypeX idx
  = TyVarX (XTyVar idx) Name
  | TyAppsX (XTyApps idx) [AppTypeX idx]
  | TyAppX (XTyApp idx) (TypeX idx) (TypeX idx)
  | TyFunX (XTyFun idx) (TypeX idx) (TypeX idx)
  | TyTupleX (XTyTuple idx) [TypeX idx]
  | TyListX (XTyList idx) (TypeX idx)
  | TypeX (XType idx)

{-
data ConstPatDetail = PrefixConPat [Pat]
                    | InfixConPat Pat Pat
-}

type family XConstPD idx
type family XPrefixCP idx
type family XInfixCP idx

data ConstPatDetailX idx
  = PrefixConPatX (XPrefixCP idx) [PatX idx]
  | InfixConPatX (XInfixCP idx) (PatX idx) (PatX idx)
  | ConstPatDetailX (XConstPD idx)

{-
data Pat = VarPat Name
             -- | ListPat
             -- | TuplePat
             | ParPat Pat
             | NPat OverLiterals
             | ConstPat Name ConstPatDetail
             | WildPat

-}

type family XPat idx
type family XVarPat idx
type family XParPat idx
type family XNPat idx
type family XConstPat idx
type family XWildPat idx

data PatX idx
  = VarPatX (XVarPat idx) Name
  | ParPatX (XParPat idx) (PatX idx)
  | NPatX (XNPat idx) (OverLiteralsX idx)
  | ConstPatX (XConstPat idx) Name (ConstPatDetailX idx)
  | WildPatX (XWildPat idx)
  | PatX (XPat idx)

{-
name m_args | m_stmts!!0 = exprs!!0
            | m_stmts!!1 = exprs!!1
            ...
            | m_stmts!!n = expts!!n
    where
      locals

data Match
  = Match {
      mArgs :: [Pat],
      mStmts :: [[Stmt]],
      mExprs :: [Expr],
      mLocals :: LocalBind}
  | Dontknow
 
-}

type family XMatch idx
type family XMatchCtr idx

data MatchX idx
  = MatchX {
      mExt :: XMatchCtr idx,
      mArgs :: [PatX idx],
      mStmts :: [[StmtX idx]],
      mExprs :: [ExprX idx],
      mLocals :: LocalBindX idx }
  | MatchX' (XMatch idx)

--newtype MatchGroup = MG [Match]

{-
data LocalBind = ValLocalBind [Bind] [Sig] | EmptyLocalBind
-}

type family XLocalBind idx
type family XValLB idx
type family XEmptyLB idx

data LocalBindX idx
  = ValLocalBindX (XValLB idx) [BindX idx] [SigX idx]
  | EmptyLocalBindX (XEmptyLB idx)
  | LocalBindX (XLocalBind idx)

{-
data Bind = FunBind {
        fun_name :: String,
        fun_matches :: [Match]
    }
  | OtherBind
-}

type family XBind idx
type family XFunBind idx

data BindX idx
  = FunBindX {
      fbExt :: XFunBind idx,
      fun_name :: String,
      fun_matches :: [MatchX idx] }
  | BindX (XBind idx)

{-
data Stmt = BindStmt Pat Expr -- TODO?
          | BodyStmt Expr     -- in guards?
          | LetStmt LocalBind -- TODO?
-}

type family XStmt idx
type family XBindStmt idx
type family XBodyStmt idx
type family XLetStmt idx

data StmtX idx = BindStmtX (XBindStmt idx) (PatX idx) (ExprX idx)
  | BodyStmtX (XBodyStmt idx) (ExprX idx)
  | LetStmtX (XLetStmt idx) (LocalBindX idx)
  | StmtX (XStmt idx)

{-
data Literals
  = LitChar Char
  | LitString String
-}

type family XLiterals idx
type family XLitChar idx
type family XLitString idx

data LiteralsX idx
  = LitCharX (XLitChar idx) Char
  | LitStringX (XLitString idx) String
  | LiteralsX (XLiterals idx)

{-
data OverLiterals
  = OverLitInteger Integer
  | OverLitFractional Rational
  | OverLitString String
-}

type family XOverLiterals idx
type family XOLInterger idx
type family XOLFractional idx
type family XOLString idx

data OverLiteralsX idx
  = OverLitIntegerX (XOLInterger idx) Integer
  | OverLitFractionaX (XOLFractional idx) Rational
  | OverLitStringX (XOLString idx) String
  | OverLiteralsX (XOverLiterals idx)


{-
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
-}

type family XExpr         idx
type family XVar          idx
type family XOverLit      idx
type family XLit          idx
type family XLam          idx
type family XApp          idx
type family XOpApp        idx
type family XLet          idx
type family XIf           idx
type family XDo           idx
type family XCase         idx
type family XExprWithType idx

data ExprX idx = VarX (XVar idx) Name
  | OverLitX      (XOverLit      idx) (OverLiteralsX idx)
  | LitX          (XLit          idx) (LiteralsX idx)
  | LamX          (XLam          idx) (MatchX idx)
  | AppX          (XApp          idx) (ExprX idx) (ExprX idx)
  | OpAppX        (XOpApp        idx) (ExprX idx) (ExprX idx) (ExprX idx)
  | LetX          (XLet          idx) (LocalBindX idx) (ExprX idx)
  | IfX           (XIf           idx) (ExprX idx) (ExprX idx) (ExprX idx)
  | DoX           (XDo           idx) [StmtX idx]
  | CaseX         (XCase         idx) (ExprX idx) (MatchX idx)
  | ExprWithTypeX (XExprWithType idx) (ExprX idx) (TypeX idx)
  | ExprX         (XExpr idx)

{-
data Decls = ValDecl Bind
           | SigDecl Sig
-}

type family XDecl    idx
type family XValDecl idx
type family XSigDecl idx

data DeclsX idx = ValDeclX (XValDecl idx) (BindX idx)
  | SigDeclX (XSigDecl idx) (SigX idx)
  | DeclsX (XDecl idx)

{-
data Hs = Hs {
    hsModuleName :: Maybe String,
    hsImports :: [Import],
    hsDecls   :: [Decls]
}
-}

type family XHs idx

data HsX idx = HsX {
  hsExt :: XHs idx,
  hsModuleName :: Maybe String,
  hsImports :: [ImportX idx],
  hsDecls   :: [DeclsX idx]
}
