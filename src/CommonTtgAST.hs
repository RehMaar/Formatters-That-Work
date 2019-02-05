{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module CommonTtgAST where

import qualified SrcLoc as Out
import qualified Language.Haskell.GHC.ExactPrint as Out

import TtgAST
import SrcInfo

data ComID

-- Extention: Common definition for source code info.

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
type instance XImportCtr ComID = Maybe SrcInfo

pattern Import :: Name -> Bool -> Maybe Name -> Maybe SrcInfo -> Import
pattern Import s q as msi = ImportX msi s q as

-- SIG

type Sig = SigX ComID
type instance XTypeSig ComID = ()
type instance XSig ComID = ()

pattern TypeSig :: [Name] -> Type -> Sig
pattern TypeSig ns t = TypeSigX () ns t

-- APP TYPE

type AppType = AppTypeX ComID
type instance XAppType   ComID = ()
type instance XAppPrefix ComID = Maybe SrcInfo
type instance XAppInfix  ComID = Maybe SrcInfo

pattern AppPrefix :: Type  -> Maybe SrcInfo -> AppType
pattern AppPrefix n msi = AppPrefixX msi n

pattern AppInfix :: Name -> Maybe SrcInfo -> AppType
pattern AppInfix t msi = AppInfixX msi t

-- TYPE

type Type = TypeX ComID
type instance XType    ComID = ()
type instance XTyVar   ComID = Maybe SrcInfo
type instance XTyApps  ComID = Maybe SrcInfo
type instance XTyApp   ComID = Maybe SrcInfo
type instance XTyFun   ComID = Maybe SrcInfo
type instance XTyTuple ComID = Maybe SrcInfo
type instance XTyList  ComID = Maybe SrcInfo

pattern TyVar :: Name -> Maybe SrcInfo -> Type
pattern TyVar n msi = TyVarX msi n

pattern TyApps :: [AppType] -> Maybe SrcInfo -> Type
pattern TyApps aps msi = TyAppsX msi aps

pattern TyApp :: Type -> Type -> Maybe SrcInfo -> Type
pattern TyApp t1 t2 msi = TyAppX msi t1 t2

pattern TyFun :: Type -> Type -> Maybe SrcInfo -> Type
pattern TyFun t1 t2 msi = TyFunX msi t1 t2

pattern TyTuple :: [Type] -> Maybe SrcInfo -> Type
pattern TyTuple ts msi = TyTupleX msi ts

pattern TyList :: Type -> Maybe SrcInfo -> Type
pattern TyList ts msi = TyListX msi ts


-- CONST PAT DETAIL

type ConstPatDetail = ConstPatDetailX ComID
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
type instance XVarPat ComID   = Maybe SrcInfo
type instance XParPat ComID   = Maybe SrcInfo
type instance XNPat ComID     = Maybe SrcInfo
type instance XConstPat ComID = Maybe SrcInfo
type instance XWildPat ComID  = Maybe SrcInfo

pattern VarPat :: Name -> Maybe SrcInfo -> Pat
pattern VarPat n msi = VarPatX msi n

pattern ParPat :: Pat -> Maybe SrcInfo -> Pat
pattern ParPat p msi = ParPatX msi p

pattern NPat :: OverLiterals -> Maybe SrcInfo -> Pat
pattern NPat ol msi = NPatX msi ol

pattern ConstPat :: Name -> ConstPatDetail -> Maybe SrcInfo -> Pat
pattern ConstPat n cpd msi = ConstPatX msi n cpd

pattern WildPat :: Maybe SrcInfo -> Pat
pattern WildPat msi = WildPatX msi

-- GHRS
type GRHS = GRHSX ComID
type instance XGRHS ComID = Maybe SrcInfo

pattern GRHS :: [Stmt] -> Expr -> Maybe SrcInfo -> GRHS
pattern GRHS s ex msi = GRHSX msi s ex

-- MATCH

type Match = MatchX ComID
type instance XMatch ComID = Maybe SrcInfo

pattern Match :: [Pat] -> [GRHS] -> LocalBind -> Maybe SrcInfo -> Match
pattern Match ps grhs lb msi = MatchX msi ps grhs lb

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
type instance XFunBind ComID = Maybe SrcInfo

pattern FunBind :: Name -> [Match] -> Maybe SrcInfo -> Bind
pattern FunBind s ms msi = FunBindX msi s ms

-- STMT

type Stmt = StmtX ComID
type instance XStmt ComID = ()
type instance XBindStmt ComID = Maybe SrcInfo
type instance XLetStmt ComID  = Maybe SrcInfo
type instance XBodyStmt ComID = Maybe SrcInfo

pattern BindStmt :: Pat -> Expr -> Maybe SrcInfo -> Stmt
pattern BindStmt p e msi = BindStmtX msi p e

pattern LetStmt :: LocalBind -> Maybe SrcInfo -> Stmt
pattern LetStmt lb msi = LetStmtX msi lb

pattern BodyStmt :: Expr -> Maybe SrcInfo -> Stmt
pattern BodyStmt e msi = BodyStmtX msi e

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
type instance XVar          ComID = Maybe SrcInfo
type instance XOverLit      ComID = Maybe SrcInfo
type instance XLit          ComID = Maybe SrcInfo
type instance XLam          ComID = Maybe SrcInfo
type instance XApp          ComID = Maybe SrcInfo
type instance XOpApp        ComID = Maybe SrcInfo
type instance XLet          ComID = Maybe SrcInfo
type instance XIf           ComID = Maybe SrcInfo
type instance XDo           ComID = Maybe SrcInfo
type instance XCase         ComID = Maybe SrcInfo
type instance XExprWithType ComID = Maybe SrcInfo

pattern Var :: Name -> Maybe SrcInfo -> Expr
pattern Var n msi = VarX msi n

pattern OverLit :: OverLiterals -> Maybe SrcInfo -> Expr
pattern OverLit ol msi = OverLitX msi ol

pattern Lit :: Literals -> Maybe SrcInfo -> Expr
pattern Lit l msi = LitX msi l

pattern Lam :: Match -> Maybe SrcInfo -> Expr
pattern Lam m msi = LamX msi m

pattern App :: Expr -> Expr -> Maybe SrcInfo -> Expr
pattern App e1 e2 msi = AppX msi e1 e2

pattern OpApp :: Expr -> Expr -> Expr -> Maybe SrcInfo -> Expr
pattern OpApp e1 op e2 msi = OpAppX msi e1 op e2

pattern Let :: LocalBind -> Expr -> Maybe SrcInfo -> Expr
pattern Let lb e msi = LetX msi lb e

pattern If :: Expr -> Expr -> Expr -> Maybe SrcInfo -> Expr
pattern If c t e msi = IfX msi c t e

pattern Do :: [Stmt] -> Maybe SrcInfo -> Expr
pattern Do s msi = DoX msi s

pattern Case :: Expr -> Match -> Maybe SrcInfo -> Expr
pattern Case e m msi = CaseX msi e m

pattern ExprWithType :: Expr -> Type -> Maybe SrcInfo -> Expr
pattern ExprWithType e t msi = ExprWithTypeX msi e t

-- DECLS

type Decls = DeclsX ComID
type instance XDecl ComID = ()
type instance XValDecl ComID = Maybe SrcInfo
type instance XSigDecl ComID = Maybe SrcInfo

pattern ValDecl :: Bind -> Maybe SrcInfo -> Decls
pattern ValDecl b msi = ValDeclX msi b

pattern SigDecl :: Sig -> Maybe SrcInfo -> Decls
pattern SigDecl s msi = SigDeclX msi s

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
deriving instance Show GRHS
deriving instance Show Match
deriving instance Show Bind
deriving instance Show Decls
deriving instance Show Import
deriving instance Show Hs
