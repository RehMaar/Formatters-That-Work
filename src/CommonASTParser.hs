{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module CommonASTParser where

import Language.Haskell.GHC.ExactPrint
import qualified GHC as G
import qualified OccName as G
import qualified RdrName as G
import qualified BasicTypes as G
import qualified Bag as G

import Language.Haskell.GHC.ExactPrint as EP
import Language.Haskell.GHC.ExactPrint.Parsers as EP
import Language.Haskell.GHC.ExactPrint.Types as EP

import qualified Data.Map.Strict as Map
import qualified FastString as FS

import Data.List

import CommonTtgAST

expr file = do
  res <- EP.parseModule file
  case res of
    Left  (src, str) -> error str
    --Right (ans, pds) -> return $ show $ getAst $ G.unLoc pds
    Right (ans, pds) -> return $ show $ getAst pds 

getAst :: G.ParsedSource -> Hs
getAst pds = f (G.unLoc pds) $ getSrcInfo pds
  where
    f = Hs <$> getModuleName <*> importsFromMod <*> getDecls

    getSrcInfo pds = Just $ SrcInfo Nothing (G.getLoc pds)

getDecls :: G.HsModule G.GhcPs -> [Decls]
getDecls hm = f <$> (G.hsmodDecls hm)
  where
    f = matchDecls <$> G.unLoc <*> getSrcInfo
    --f = matchDecls . G.unLoc -- <*> getSrcInfo

-- Module name
--getModuleName :: G.HsModule G.GhcPs -> Maybe Name
getModuleName h = f <$> G.hsmodName h
  where
    f = Name <$> (G.moduleNameString . G.unLoc) <*> getSrcInfo

-- FunBind name
getFunBindIdName = Name <$> (handleFunBindId . G.unLoc) <*> getSrcInfo

-- Imports
getImports :: G.Located (G.ImportDecl a) -> Import
getImports h =
  let hl = G.unLoc h
      name        = Name (G.moduleNameString $ G.unLoc $ G.ideclName hl) (getSrcInfo $ G.ideclName hl)
      isQuilified = G.ideclQualified hl
      alias       = (Name <$> (G.moduleNameString <$> G.unLoc) <*> getSrcInfo) <$> (G.ideclAs hl)
      srcinfo     = G.getLoc h
  in  Import name isQuilified alias

importsFromMod :: G.HsModule name -> [Import]
importsFromMod h = getImports <$> {-G.unLoc <$>-} G.hsmodImports h

getSrcInfo = Just . SrcInfo Nothing . G.getLoc

matchPatSI = matchPat <$> G.unLoc <*> getSrcInfo

matchPat :: G.Pat G.GhcPs -> Maybe SrcInfo -> Pat
matchPat (G.WildPat G.PlaceHolder) = WildPat
matchPat (G.VarPat  name         ) = VarPat $ getFunBindIdName name
matchPat (G.ConPatIn name det    ) = ConstPat (getFunBindIdName name) (matchDetails det)
  where
    matchDetails (G.PrefixCon args) = PrefixConPat (matchPatSI <$> args)
    matchDetails (G.InfixCon a1 a2) = InfixConPat (matchPatSI a1) (matchPatSI a2)

matchPat (G.NPat lit _ _ _) = NPat $ matchOverLit $ G.unLoc lit
matchPat (G.ParPat pat    ) = ParPat $ error "ParPat"
matchPat _                  = error "unknown pattern"

-- ParStmt -- for list compreh
matchStmt :: G.ExprStmt G.GhcPs -> Maybe SrcInfo -> Stmt
matchStmt (G.LetStmt bind       ) = LetStmt $ matchLocalFunBind $ G.unLoc bind
matchStmt (G.BodyStmt body _ _ _) = BodyStmt $ matchExprSI body
matchStmt (G.BindStmt pat body _ _ _) =
  BindStmt (error "BindStmt 1") (error "BindStmt 2")
matchStmt _ = error "unknown statement"

matchLit :: G.HsLit x -> Literals
matchLit (G.HsChar   _ c) = LitChar c
matchLit (G.HsString _ s) = LitString $ FS.unpackFS s
matchLit _                = error "unsupported literal type"

matchOverLit :: G.HsOverLit G.GhcPs -> OverLiterals
matchOverLit (G.OverLit val _ _ _) = handleOL val
 where
  handleOL (G.HsIntegral   l) = OverLitInteger $ handleInt l
  handleOL (G.HsFractional l) = OverLitFractional $ handleFrac l
  handleOL (G.HsIsString t s) = OverLitString $ FS.unpackFS s

  handleInt (G.IL _ _ val) = val
  handleFrac (G.FL _ _ val) = val

matchExprSI :: G.LHsExpr G.GhcPs -> Expr
matchExprSI = matchExpr <$> G.unLoc <*> getSrcInfo

matchExpr :: G.HsExpr G.GhcPs -> Maybe SrcInfo -> Expr
matchExpr (G.HsVar     name) = Var (getFunBindIdName name)
matchExpr (G.HsLit     lit ) = Lit (matchLit lit)
matchExpr (G.HsOverLit name) = OverLit (matchOverLit name)
matchExpr (G.HsApp expr1 expr2) =
  App (matchExprSI expr1)
      (matchExprSI expr2)
matchExpr (G.OpApp le op _ re) = OpApp (matchExprSI le)
                                       (matchExprSI op)
                                       (matchExprSI re)
matchExpr (G.HsIf _ ife thene elsee) = If (matchExprSI ife)
                                          (matchExprSI thene)
                                          (matchExprSI elsee)
matchExpr (G.HsLet locs expr) =
  Let (matchLocalFunBind $ G.unLoc locs) (matchExprSI expr)
matchExpr (G.ExprWithTySig expr typ) =
  ExprWithType (matchExprSI expr) (matchSigWcType typ)
matchExpr g = \_ -> error $ "unsupported " ++ ctorExprPrint g

matchLocalFunBind (G.HsValBinds (G.ValBindsIn binds sigs)) =
  let types = (matchSigBind . G.unLoc) <$> sigs
      vals  = (matchFunBind . G.unLoc) <$> G.bagToList binds
      --vals  = (matchFunBind <$> G.unLoc <*> getSrcInfo) <$> G.bagToList binds
  in  ValLocalBind vals types
  where valBinds binds = undefined
      --  getSrcInfo = SrcInfo Nothing . G.getLoc
matchLocalFunBind G.EmptyLocalBinds = EmptyLocalBind
matchLocalFunBind _                 = error "unsupported LocalBind ctor!"

--matchFunBind :: G.HsBindLR id G.GhcPs -> Decls
matchFunBind (G.FunBind id match _ _ _) = FunBind
  (getFunBindIdName id)
  (matchMatchGroup match)
  (getSrcInfo id)
 where
  matchMatchGroup (G.MG alts _ _ _) = (matchMatch . G.unLoc) <$> (G.unLoc alts)
  matchMatch (G.Match ctx pats grhs) =
    let argsp   = matchPatSI <$> pats
        exprss  = (getExprs . G.unLoc) <$> (dectrGRHSSExprs grhs)
        stmts   = (getStmts . G.unLoc) <$> (dectrGRHSSExprs grhs)
        localsb = getLocals $ G.unLoc (dectrGRHSSLocals grhs)
    in  Match argsp stmts exprss localsb
   where
    dectrGRHSSExprs (G.GRHSs bod loc) = bod
    dectrGRHSSLocals (G.GRHSs bod loc) = loc
    getExprs (G.GRHS stmts body) = matchExprSI body
    getStmts (G.GRHS stmts body) = getStmts' stmts

    getLocals = matchLocalFunBind

    getStmts' []     = [] --error "Empty"
    getStmts' (e:[]) = [matchStmt (G.unLoc e) (getSrcInfo e)]
    getStmts' (e:es) = error "I'm not the only one!"
matchFunBind _ = error "Other sig"

--matchSigWcType :: SOLONGTYPE -> Type
matchSigWcType (G.HsWC _ (G.HsIB _ t _)) = matchTypSI t

matchTypSI = matchTyp <$> G.unLoc <*> getSrcInfo

matchTyp :: G.HsType G.GhcPs -> Maybe SrcInfo -> Type
matchTyp (G.HsTyVar _ l) = TyVar $ getFunBindIdName l
matchTyp (G.HsAppsTy at) = TyApps $ appTypSI <$> at
 where
  appTypSI = apptyp <$> G.unLoc <*> getSrcInfo
  apptyp (G.HsAppInfix  l) = AppInfix $ getFunBindIdName l
  apptyp (G.HsAppPrefix t) = AppPrefix $ matchTypSI t
matchTyp (G.HsAppTy t1 t2) =
  TyApp (matchTypSI t1) (matchTypSI t2)
matchTyp (G.HsFunTy t1 t2) =
  TyFun (matchTypSI t1) (matchTypSI t2)
matchTyp (G.HsTupleTy _ tps) = error "Turple in types!"
matchTyp (G.HsListTy tp    ) = TyList $ matchTypSI tp
matchTyp _                   = error "unknown type"

matchSigBind (G.TypeSig id thing) = Type (names id) (sigs thing)
 where
  names = fmap getFunBindIdName
  sigs (G.HsWC _ (G.HsIB _ b _)) = matchTypSI b
matchSigBind _ = error "unknown sig"

matchDecls :: G.HsDecl G.GhcPs -> Maybe SrcInfo -> Decls
matchDecls (G.ValD a) = ValDecl $ matchFunBind a
matchDecls (G.SigD a) = SigDecl $ matchSigBind a
matchDecls _          = error "unknown decl "

handleFunBindId (G.Unqual name) = G.occNameString name
handleFunBindId (G.Qual mod name) =
  (G.moduleNameString mod) ++ "." ++ (G.occNameString name)
handleFunBindId (G.Orig mod name) = error "unknown name of type `Orig'"
handleFunBindId (G.Exact name   ) = error "unknown name of type `Exac't"

-- HELPER

ctorExprPrint (G.HsVar        _       ) = "HsVar"
ctorExprPrint (G.HsUnboundVar _       ) = "HsUnboundVar"
ctorExprPrint (G.HsConLikeOut _       ) = "HsConLikeOut"
ctorExprPrint (G.HsRecFld     _       ) = "HsRecFld"
ctorExprPrint (G.HsOverLabel _ _      ) = "HsOverLabel"
ctorExprPrint (G.HsIPVar   _          ) = "HsIPVar"
ctorExprPrint (G.HsOverLit _          ) = "HsOverLit"
ctorExprPrint (G.HsLit     _          ) = "HsLit"
ctorExprPrint (G.HsLam     _          ) = "HsLam"
ctorExprPrint (G.HsLamCase _          ) = "HsLamCase"
ctorExprPrint (G.HsApp        _ _     ) = "HsApp"
ctorExprPrint (G.HsAppType    _ _     ) = "HsAppType"
ctorExprPrint (G.HsAppTypeOut _ _     ) = "HsAppTypeOut"
ctorExprPrint (G.OpApp _ _ _ _        ) = "OpApp"
ctorExprPrint (G.NegApp _ _           ) = "NegApp"
ctorExprPrint (G.HsPar _              ) = "HsPar"
ctorExprPrint (G.SectionL      _ _    ) = "SectionL"
ctorExprPrint (G.SectionR      _ _    ) = "SectionR"
ctorExprPrint (G.ExplicitTuple _ _    ) = "ExplicitTuple"
ctorExprPrint (G.ExplicitSum _ _ _ _  ) = "ExplicitSum"
ctorExprPrint (G.HsCase    _ _        ) = "HsCase"
ctorExprPrint (G.HsMultiIf _ _        ) = "HsMultiIf"
ctorExprPrint (G.HsLet     _ _        ) = "HsLet"
ctorExprPrint (G.HsDo         _ _ _   ) = "HsDo"
ctorExprPrint (G.ExplicitList _ _ _   ) = "ExplicitList"
ctorExprPrint (G.ExplicitPArr _ _     ) = "ExplicitPArr"
ctorExprPrint (G.RecordCon _ _ _ _    ) = "RecordCon"
ctorExprPrint (G.RecordUpd _ _ _ _ _ _) = "RecordUpd"
ctorExprPrint (G.ExprWithTySig    _ _ ) = "ExprWithTySig"
ctorExprPrint (G.ExprWithTySigOut _ _ ) = "ExprWithTySigOut"
ctorExprPrint (G.ArithSeq _ _ _       ) = "ArithSeq"
ctorExprPrint (G.PArrSeq _ _          ) = "PArrSeq"
ctorExprPrint (G.HsSCC     _ _ _      ) = "HsSCC"
ctorExprPrint (G.HsCoreAnn _ _ _      ) = "HsCoreAnn"
ctorExprPrint (G.HsBracket _          ) = "HsBracket"
ctorExprPrint (G.HsRnBracketOut _ _   ) = "HsRnBracketOut"
ctorExprPrint (G.HsTcBracketOut _ _   ) = "HsTcBracketOut"
ctorExprPrint (G.HsSpliceE _          ) = "HsSpliceE"
ctorExprPrint (G.HsProc   _ _         ) = "HsProc"
ctorExprPrint (G.HsStatic _ _         ) = "HsStatic"
ctorExprPrint (G.HsArrApp _ _ _ _ _   ) = "HsArrApp"
ctorExprPrint (G.HsArrForm _ _ _      ) = "HsArrForm"
ctorExprPrint (G.HsTick _ _           ) = "HsTick"
ctorExprPrint (G.HsBinTick _ _ _      ) = "HsBinTick"
ctorExprPrint (G.HsTickPragma _ _ _ _ ) = "HsTickPragma"
ctorExprPrint (G.EWildPat             ) = "EWildPat"
ctorExprPrint (G.EAsPat   _ _         ) = "EAsPat"
ctorExprPrint (G.EViewPat _ _         ) = "EViewPat"
ctorExprPrint (G.ELazyPat _           ) = "ELazyPat"
ctorExprPrint (G.HsWrap _ _           ) = "HsWrap"
