{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AnnTtgAST where

import qualified SrcLoc as Out
import qualified Language.Haskell.GHC.ExactPrint as Out

import SrcInfo
import TtgAST

data AnnID

-- Extention: Common definition for source code info.

type SrcInfo = SrcInfoX AnnID
type instance XSrcInfoAnn AnnID = Maybe Out.Annotation
type instance XSrcInfoLoc AnnID = ()

pattern SrcInfo :: Maybe Out.Annotation -> SrcInfo
pattern SrcInfo ma = SrcInfoX ma ()

deriving instance Show SrcInfo

-- Helpers
type Name = NameX AnnID
type instance XName AnnID = Maybe SrcInfo

pattern Name :: String -> Maybe SrcInfo -> Name
pattern Name s si = NameX si s

-- AST Definition

-- IMPORT

type Import = ImportX AnnID
type instance XImportCtr AnnID = Maybe SrcInfo

pattern Import :: Name -> Bool -> Maybe Name -> Maybe SrcInfo -> Import
pattern Import s q as msi = ImportX msi s q as

-- SIG

type Sig= SigX AnnID
type instance XTypeSig AnnID = Maybe SrcInfo
type instance XSig AnnID = ()

pattern TypeSig :: [Name] -> Type -> Maybe SrcInfo -> Sig
pattern TypeSig  ns t msi = TypeSigX msi ns t

-- APP TYPE

type AppType = AppTypeX AnnID
type instance XAppType   AnnID = ()
type instance XAppPrefix AnnID = Maybe SrcInfo
type instance XAppInfix  AnnID = Maybe SrcInfo

pattern AppPrefix :: Type  -> Maybe SrcInfo -> AppType
pattern AppPrefix n msi = AppPrefixX msi n

pattern AppInfix :: Name -> Maybe SrcInfo -> AppType
pattern AppInfix t msi = AppInfixX msi t

-- TYPE

type Type = TypeX AnnID
type instance XType    AnnID = ()
type instance XTyVar   AnnID = Maybe SrcInfo
type instance XTyApps  AnnID = Maybe SrcInfo
type instance XTyApp   AnnID = Maybe SrcInfo
type instance XTyFun   AnnID = Maybe SrcInfo
type instance XTyTuple AnnID = Maybe SrcInfo
type instance XTyList  AnnID = Maybe SrcInfo

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

type ConstPatDetail = ConstPatDetailX AnnID
type instance XConstPD AnnID = ()
type instance XPrefixCP AnnID = ()
type instance XInfixCP AnnID = ()

pattern PrefixConPat :: [Pat] -> ConstPatDetail
pattern PrefixConPat ps = PrefixConPatX () ps

pattern InfixConPat :: Pat -> Pat -> ConstPatDetail
pattern InfixConPat p1 p2 = InfixConPatX () p1 p2

-- PAT

type Pat = PatX AnnID
type instance XPat AnnID = ()
type instance XVarPat AnnID   = Maybe SrcInfo
type instance XParPat AnnID   = Maybe SrcInfo
type instance XNPat AnnID     = Maybe SrcInfo
type instance XConstPat AnnID = Maybe SrcInfo
type instance XWildPat AnnID  = Maybe SrcInfo

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

-- GRHS

type GRHS = GRHSX AnnID
type instance XGRHS AnnID = Maybe SrcInfo

pattern GRHS :: [Stmt] -> Expr -> Maybe SrcInfo -> GRHS
pattern GRHS s e ms = GRHSX ms s e

-- MATCH

type Match = MatchX AnnID
type instance XMatch AnnID = Maybe SrcInfo

pattern Match :: [Pat] -> [GRHS] -> LocalBind -> Maybe SrcInfo -> Match
pattern Match ps grhs lb msi = MatchX msi ps grhs lb

-- LOCAL BIND

type LocalBind = LocalBindX AnnID
type instance XLocalBind AnnID = ()
type instance XValLB AnnID = ()
type instance XEmptyLB AnnID = ()

pattern ValLocalBind :: [Bind] -> [Sig] -> LocalBind
pattern ValLocalBind bs ss = ValLocalBindX () bs ss

pattern EmptyLocalBind :: LocalBind
pattern EmptyLocalBind = EmptyLocalBindX ()

-- BIND

type Bind = BindX AnnID
type instance XBind AnnID = ()
type instance XFunBind AnnID = Maybe SrcInfo

pattern FunBind :: Name -> [Match] -> Maybe SrcInfo -> Bind
pattern FunBind s ms msi = FunBindX msi s ms

-- STMT

type Stmt = StmtX AnnID
type instance XStmt AnnID = ()
type instance XBindStmt AnnID = Maybe SrcInfo
type instance XLetStmt AnnID  = Maybe SrcInfo
type instance XBodyStmt AnnID = Maybe SrcInfo

pattern BindStmt :: Pat -> Expr -> Maybe SrcInfo -> Stmt
pattern BindStmt p e msi = BindStmtX msi p e

pattern LetStmt :: LocalBind -> Maybe SrcInfo -> Stmt
pattern LetStmt lb msi = LetStmtX msi lb

pattern BodyStmt :: Expr -> Maybe SrcInfo -> Stmt
pattern BodyStmt e msi = BodyStmtX msi e

-- LITERALS
type Literals = LiteralsX AnnID
type instance XLiterals AnnID = ()
type instance XLitChar AnnID = ()
type instance XLitString AnnID = ()

pattern LitChar :: Char -> Literals
pattern LitChar c = LitCharX () c

pattern LitString :: String -> Literals
pattern LitString s = LitStringX () s

-- OVERLITERALS

type OverLiterals = OverLiteralsX AnnID
type instance XOverLiterals AnnID = ()
type instance XOLInterger   AnnID = ()
type instance XOLFractional AnnID = ()
type instance XOLString     AnnID = ()

pattern OverLitInteger :: Integer -> OverLiterals
pattern OverLitInteger i = OverLitIntegerX () i

pattern OverLitFractional :: Rational -> OverLiterals
pattern OverLitFractional r = OverLitFractionalX () r

pattern OverLitString :: String -> OverLiterals
pattern OverLitString s = OverLitStringX () s

-- EXPR

type Expr = ExprX AnnID
type instance XExpr         AnnID = ()
type instance XVar          AnnID = Maybe SrcInfo
type instance XOverLit      AnnID = Maybe SrcInfo
type instance XLit          AnnID = Maybe SrcInfo
type instance XLam          AnnID = Maybe SrcInfo
type instance XApp          AnnID = Maybe SrcInfo
type instance XOpApp        AnnID = Maybe SrcInfo
type instance XLet          AnnID = Maybe SrcInfo
type instance XIf           AnnID = Maybe SrcInfo
type instance XDo           AnnID = Maybe SrcInfo
type instance XCase         AnnID = Maybe SrcInfo
type instance XExprWithType AnnID = Maybe SrcInfo
type instance XPar          AnnID = Maybe SrcInfo

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

pattern Par :: Expr -> Maybe SrcInfo -> Expr
pattern Par e msi = ParX msi e

-- DECLS

type Decls = DeclsX AnnID
type instance XDecl AnnID = ()
type instance XValDecl AnnID = Maybe SrcInfo
type instance XSigDecl AnnID = Maybe SrcInfo

pattern ValDecl :: Bind -> Maybe SrcInfo -> Decls
pattern ValDecl b msi = ValDeclX msi b

pattern SigDecl :: Sig -> Maybe SrcInfo -> Decls
pattern SigDecl s msi = SigDeclX msi s

-- HS

type Hs = HsX AnnID
type instance XHs AnnID = Maybe SrcInfo

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
