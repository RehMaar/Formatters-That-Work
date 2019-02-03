{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module CommonTtgAST where

import qualified SrcLoc as Out
import qualified Language.Haskell.GHC.ExactPrint as Out

import TtgAST


data ComID

-- Extention: Common definition for source code info.

type family XSrcInfoAnn idx
type family XSrcInfoLoc idx
data SrcInfoX idx = SrcInfoX (XSrcInfoAnn idx) (XSrcInfoLoc idx)

type SrcInfo = SrcInfoX ComID
type instance XSrcInfoAnn ComID = Maybe Out.Annotation
type instance XSrcInfoLoc ComID = Out.SrcSpan

pattern SrcInfo :: Maybe Out.Annotation -> Out.SrcSpan -> SrcInfo
pattern SrcInfo ma sl = SrcInfoX ma sl

deriving instance Show SrcInfo

-- Helpers
type Name = NameX ComID
type instance XName ComID = Maybe SrcInfo

pattern Name :: String -> Maybe SrcInfo -> Name
pattern Name s si = NameX si s
{-
type Name = NameX ComID
type instance XName ComID = ()

pattern Name :: String -> Name
pattern Name s = NameX () s
-}
-- AST Definition

-- IMPORT

type Import = ImportX ComID
type instance XImportCtr ComID = ()

pattern Import :: Name -> Bool -> Maybe Name -> Import
pattern Import s q as = ImportX () s q as

-- SIG

type Sig = SigX ComID
type instance XTypeSig ComID = ()
type instance XSig ComID = ()

pattern Type :: [Name] -> Type -> Sig
pattern Type ns t = TypeSigX () ns t

-- APP TYPE

type AppType = AppTypeX ComID
type instance XAppType ComID = ()
type instance XAppPrefix ComID = ()
type instance XAppInfix ComID = ()

pattern AppPrefix :: Type  -> AppType
pattern AppPrefix n = AppPrefixX () n

pattern AppInfix :: Name -> AppType
pattern AppInfix t = AppInfixX () t

-- TYPE

type Type = TypeX ComID
type instance XType ComID = ()
type instance XTyVar ComID = ()
type instance XTyApps ComID = ()
type instance XTyApp ComID = ()
type instance XTyFun ComID = ()
type instance XTyTuple ComID = ()
type instance XTyList ComID = ()

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

type ConstPatDetail = ConstPatDetailX ComID
type instance XConstPat ComID = ()
type instance XConstPD ComID = ()
type instance XPrefixCP ComID = ()
type instance XInfixCP ComID = ()

pattern PrefixConPat :: [Pat] -> ConstPatDetail
pattern PrefixConPat ps = PrefixConPatX () ps

pattern InfixConPat :: Pat -> Pat -> ConstPatDetail
pattern InfixConPat p1 p2 = InfixConPatX () p1 p2

-- PAT

type Pat = PatX ComID
type instance XPat ComID = ()
type instance XVarPat ComID = ()
type instance XParPat ComID = ()
type instance XNPat ComID = ()
type instance XConstPat ComID = ()
type instance XWildPat ComID = ()

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

-- MATCH

type Match = MatchX ComID
type instance XMatch ComID = ()
type instance XMatchCtr ComID = ()

pattern Match :: [Pat] -> [[Stmt]] -> [Expr] -> LocalBind -> Match
pattern Match ps sss es lb = MatchX () ps sss es lb

-- LOCAL BIND

type LocalBind = LocalBindX ComID
type instance XLocalBind ComID = ()
type instance XValLB ComID = ()
type instance XEmptyLB ComID = ()

pattern ValLocalBind :: [Bind] -> [Sig] -> LocalBind
pattern ValLocalBind bs ss = ValLocalBindX () bs ss

pattern EmptyLocalBind :: LocalBind
pattern EmptyLocalBind = EmptyLocalBindX ()

-- BIND

type Bind = BindX ComID
type instance XBind ComID = ()
type instance XFunBind ComID = ()

pattern FunBind :: Name -> [Match] -> Bind
pattern FunBind s ms = FunBindX () s ms

-- STMT

type Stmt = StmtX ComID
type instance XStmt ComID = ()
type instance XBindStmt ComID = ()
type instance XLetStmt ComID = ()
type instance XBodyStmt ComID = ()

pattern BindStmt :: Pat -> Expr -> Stmt
pattern BindStmt p e = BindStmtX () p e

pattern LetStmt :: LocalBind -> Stmt
pattern LetStmt lb = LetStmtX () lb

pattern BodyStmt :: Expr -> Stmt
pattern BodyStmt e = BodyStmtX () e

-- LITERALS
type Literals = LiteralsX ComID
type instance XLiterals ComID = ()
type instance XLitChar ComID = ()
type instance XLitString ComID = ()

pattern LitChar :: Char -> Literals
pattern LitChar c = LitCharX () c

pattern LitString :: String -> Literals
pattern LitString s = LitStringX () s

-- OVERLITERALS

type OverLiterals = OverLiteralsX ComID
type instance XOverLiterals ComID = ()
type instance XOLInterger ComID = ()
type instance XOLFractional ComID = ()
type instance XOLString ComID = ()

pattern OverLitInteger :: Integer -> OverLiterals
pattern OverLitInteger i = OverLitIntegerX () i

pattern OverLitFractional :: Rational -> OverLiterals
pattern OverLitFractional r = OverLitFractionalX () r

pattern OverLitString :: String -> OverLiterals
pattern OverLitString s = OverLitStringX () s

-- EXPR

type Expr = ExprX ComID
type instance XExpr         ComID = ()
type instance XVar          ComID = ()
type instance XOverLit      ComID = ()
type instance XLit          ComID = ()
type instance XLam          ComID = ()
type instance XApp          ComID = ()
type instance XOpApp        ComID = ()
type instance XLet          ComID = ()
type instance XIf           ComID = ()
type instance XDo           ComID = ()
type instance XCase         ComID = ()
type instance XExprWithType ComID = ()

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

-- DECLS

type Decls = DeclsX ComID
type instance XDecl ComID = ()
type instance XValDecl ComID = () --SrcInfo
type instance XSigDecl ComID = () --SrcInfo

pattern ValDecl :: Bind -> Decls
pattern ValDecl b = ValDeclX () b

pattern SigDecl :: Sig -> Decls
pattern SigDecl s = SigDeclX () s

-- HS

type Hs = HsX ComID
type instance XHs ComID = Maybe SrcInfo

pattern Hs :: Maybe Name -> [Import] -> [Decls] -> Maybe SrcInfo -> Hs
pattern Hs n i d s = HsX s n i d

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
deriving instance Show Match
deriving instance Show Bind
deriving instance Show Decls
deriving instance Show Import
deriving instance Show Hs
