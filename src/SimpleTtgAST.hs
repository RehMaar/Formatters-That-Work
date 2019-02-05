{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleTtgAST where

import TtgAST

data UD

-- Name

type Name = NameX UD
type instance XName UD = ()

pattern Name :: String -> Name
pattern Name s = NameX () s

-- IMPORT

type Import = ImportX UD
type instance XImportCtr UD = ()

pattern Import :: Name -> Bool -> Maybe Name -> Import
pattern Import s q as = ImportX () s q as

-- SIG

type Sig = SigX UD
type instance XTypeSig UD = ()
type instance XSig UD = ()

pattern TypeSig :: [Name] -> Type -> Sig
pattern TypeSig ns t = TypeSigX () ns t


-- APP TYPE

type AppType = AppTypeX UD
type instance XAppType UD = ()
type instance XAppPrefix UD = ()
type instance XAppInfix UD = ()

pattern AppPrefix :: Type  -> AppType
pattern AppPrefix n = AppPrefixX () n

pattern AppInfix :: Name -> AppType
pattern AppInfix t = AppInfixX () t

-- TYPE

type Type = TypeX UD
type instance XType UD = ()
type instance XTyVar UD = ()
type instance XTyApps UD = ()
type instance XTyApp UD = ()
type instance XTyFun UD = ()
type instance XTyTuple UD = ()
type instance XTyList UD = ()

pattern TyVar :: Name -> Type
pattern TyVar n = TyVarX () n

pattern TyApps :: [AppType] -> Type
pattern TyApps aps = TyAppsX () aps

pattern TyApp :: Type -> Type -> Type
pattern TyApp t1 t2 = TyAppX () t1 t2

pattern TyFun :: Type -> Type -> Type
pattern TyFun t1 t2 = TyFunX () t1 t2

pattern TyTuple :: [Type] -> Type
pattern TyTuple ts = TyTupleX () ts

pattern TyList :: Type -> Type
pattern TyList ts = TyListX () ts


-- CONST PAT DETAIL

type ConstPatDetail = ConstPatDetailX UD
type instance XConstPat UD = ()
type instance XConstPD UD = ()
type instance XPrefixCP UD = ()
type instance XInfixCP UD = ()

pattern PrefixConPat :: [Pat] -> ConstPatDetail
pattern PrefixConPat ps = PrefixConPatX () ps

pattern InfixConPat :: Pat -> Pat -> ConstPatDetail
pattern InfixConPat p1 p2 = InfixConPatX () p1 p2

-- PAT

type Pat = PatX UD
type instance XPat UD = ()
type instance XVarPat UD = ()
type instance XParPat UD = ()
type instance XNPat UD = ()
type instance XConstPat UD = ()
type instance XWildPat UD = ()

pattern VarPat :: Name -> Pat
pattern VarPat n = VarPatX () n

pattern ParPat :: Pat -> Pat
pattern ParPat p = ParPatX () p

pattern NPat :: OverLiterals -> Pat
pattern NPat ol = NPatX () ol

pattern ConstPat :: Name -> ConstPatDetail -> Pat
pattern ConstPat n cpd = ConstPatX () n cpd

pattern WildPat :: Pat
pattern WildPat = WildPatX ()

-- GRHS

type GRHS = GRHSX UD
type instance XGRHS UD = ()

pattern GRHS :: [Stmt] -> Expr -> GRHS
pattern GRHS s e = GRHSX () s e

-- MATCH

type Match = MatchX UD
type instance XMatch UD = ()

pattern Match :: [Pat] -> [GRHS] -> LocalBind -> Match
pattern Match ps grhs lb = MatchX () ps grhs lb

-- LOCAL BIND

type LocalBind = LocalBindX UD
type instance XLocalBind UD = ()
type instance XValLB UD = ()
type instance XEmptyLB UD = ()

pattern ValLocalBind :: [Bind] -> [Sig] -> LocalBind
pattern ValLocalBind bs ss = ValLocalBindX () bs ss

pattern EmptyLocalBind :: LocalBind
pattern EmptyLocalBind = EmptyLocalBindX ()

-- BIND

type Bind = BindX UD
type instance XBind UD = ()
type instance XFunBind UD = ()

pattern FunBind :: Name -> [Match] -> Bind
pattern FunBind s ms = FunBindX () s ms

-- STMT

type Stmt = StmtX UD
type instance XStmt UD = ()
type instance XBindStmt UD = ()
type instance XLetStmt UD = ()
type instance XBodyStmt UD = ()

pattern BindStmt :: Pat -> Expr -> Stmt
pattern BindStmt p e = BindStmtX () p e

pattern LetStmt :: LocalBind -> Stmt
pattern LetStmt lb = LetStmtX () lb

pattern BodyStmt :: Expr -> Stmt
pattern BodyStmt e = BodyStmtX () e

-- LITERALS
type Literals = LiteralsX UD
type instance XLiterals UD = ()
type instance XLitChar UD = ()
type instance XLitString UD = ()

pattern LitChar :: Char -> Literals
pattern LitChar c = LitCharX () c

pattern LitString :: String -> Literals
pattern LitString s = LitStringX () s

-- OVERLITERALS

type OverLiterals = OverLiteralsX UD
type instance XOverLiterals UD = ()
type instance XOLInterger UD = ()
type instance XOLFractional UD = ()
type instance XOLString UD = ()

pattern OverLitInteger :: Integer -> OverLiterals
pattern OverLitInteger i = OverLitIntegerX () i

pattern OverLitFractional :: Rational -> OverLiterals
pattern OverLitFractional r = OverLitFractionalX () r

pattern OverLitString :: String -> OverLiterals
pattern OverLitString s = OverLitStringX () s

-- EXPR

type Expr = ExprX UD
type instance XExpr         UD = ()
type instance XVar          UD = ()
type instance XOverLit      UD = ()
type instance XLit          UD = ()
type instance XLam          UD = ()
type instance XApp          UD = ()
type instance XOpApp        UD = ()
type instance XLet          UD = ()
type instance XIf           UD = ()
type instance XDo           UD = ()
type instance XCase         UD = ()
type instance XExprWithType UD = ()
type instance XPar          UD = ()

pattern Var :: Name -> Expr
pattern Var n = VarX () n

pattern OverLit :: OverLiterals -> Expr
pattern OverLit ol = OverLitX () ol

pattern Lit :: Literals -> Expr
pattern Lit l = LitX () l

pattern Lam :: Match -> Expr
pattern Lam m = LamX () m

pattern App :: Expr -> Expr -> Expr
pattern App e1 e2 = AppX () e1 e2

pattern OpApp :: Expr -> Expr -> Expr -> Expr
pattern OpApp e1 op e2 = OpAppX () e1 op e2

pattern Let :: LocalBind -> Expr -> Expr
pattern Let lb e = LetX () lb e

pattern If :: Expr -> Expr -> Expr -> Expr
pattern If c t e = IfX () c t e

pattern Do :: [Stmt] -> Expr
pattern Do s = DoX () s

pattern Case :: Expr -> Match -> Expr
pattern Case e m = CaseX () e m

pattern ExprWithType :: Expr -> Type -> Expr
pattern ExprWithType e t = ExprWithTypeX () e t

pattern Par :: Expr -> Expr
pattern Par e = ParX () e

-- DECLS

type Decls = DeclsX UD
type instance XDecl UD = ()
type instance XValDecl UD = ()
type instance XSigDecl UD = ()

pattern ValDecl :: Bind -> Decls
pattern ValDecl b = ValDeclX () b

pattern SigDecl :: Sig -> Decls
pattern SigDecl s = SigDeclX () s

-- HS

type Hs = HsX UD
type instance XHs UD = ()

pattern Hs :: Maybe Name -> [Import] -> [Decls] -> Hs
pattern Hs n i d = HsX () n i d

deriving instance Show Name
deriving instance Show AppType
deriving instance Show Type
deriving instance Show Sig
deriving instance Show ConstPatDetail
deriving instance Show Stmt
deriving instance Show Pat
deriving instance Show Literals
deriving instance Show OverLiterals
deriving instance Show Expr
deriving instance Show LocalBind
deriving instance Show GRHS
deriving instance Show Match
deriving instance Show Bind
deriving instance Show Decls
deriving instance Show Import
deriving instance Show Hs
