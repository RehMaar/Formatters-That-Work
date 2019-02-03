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

import qualified Data.Data as DD

import Data.List

import CommonTtgAST

getSrcInfo :: DD.Data a => EP.Anns -> G.Located a -> Maybe SrcInfo
getSrcInfo ans loc = Just (SrcInfo (getAnn loc) (G.getLoc loc))
  where getAnn loc = Map.lookup (EP.mkAnnKey loc) ans

expr file = do
  res <- EP.parseModule file
  case res of
    Left  (src, str) -> error str
    Right (ans, pds) -> return $ show $ getAst ans pds

parseAST file =
  do
    res <- EP.parseModule file
    return $ case res of
      Left  (src, str) -> Left str
      Right (ans, pds) -> Right $ getAst ans pds

getAst :: EP.Anns -> G.ParsedSource -> Hs
getAst ans = f <$> G.unLoc <*> getSrcInfo ans
  where
    f = Hs <$> getModuleName ans <*> importsFromMod ans <*> getDecls ans

getDecls :: EP.Anns -> G.HsModule G.GhcPs -> [Decls]
getDecls ans hm = f <$> (G.hsmodDecls hm)
  where
    f = matchDecls ans <$> G.unLoc <*> getSrcInfo ans

-- Module name
--getModuleName :: G.HsModule G.GhcPs -> Maybe Name
getModuleName ans h = f <$> G.hsmodName h
  where
    f = Name <$> (G.moduleNameString . G.unLoc) <*> getSrcInfo ans

-- FunBind name
getFunBindIdName ans = Name <$> (handleFunBindId . G.unLoc) <*> getSrcInfo ans

-- Imports
getImports ans h =
  let hl = G.unLoc h
      name        = Name (G.moduleNameString $ G.unLoc $ G.ideclName hl) (getSrcInfo ans $ G.ideclName hl)
      isQuilified = G.ideclQualified hl
      alias       = (Name <$> (G.moduleNameString <$> G.unLoc) <*> getSrcInfo ans) <$> (G.ideclAs hl)
  in  Import name isQuilified alias (getSrcInfo ans h)

importsFromMod ans h = getImports ans <$> {-G.unLoc <$>-} G.hsmodImports h

matchPatSI ans = matchPat ans <$> G.unLoc <*> getSrcInfo ans

matchPat :: EP.Anns -> G.Pat G.GhcPs -> Maybe SrcInfo -> Pat
matchPat ans (G.WildPat G.PlaceHolder) = WildPat
matchPat ans (G.VarPat  name         ) = VarPat $ getFunBindIdName ans name
matchPat ans (G.ConPatIn name det    ) = ConstPat (getFunBindIdName ans name) (matchDetails ans det)
  where
    matchDetails ans (G.PrefixCon args) = PrefixConPat (matchPatSI ans <$> args)
    matchDetails ans (G.InfixCon a1 a2) = InfixConPat (matchPatSI ans a1) (matchPatSI ans a2)

matchPat ans (G.NPat lit _ _ _) = NPat $ matchOverLit $ G.unLoc lit
matchPat ans (G.ParPat pat    ) = ParPat $ error "ParPat"
matchPat _ _                    = \_ -> error "unknown pattern"

-- ParStmt -- for list compreh
matchStmt :: EP.Anns -> G.ExprStmt G.GhcPs -> Maybe SrcInfo -> Stmt
matchStmt ans (G.LetStmt bind       ) = LetStmt $ matchLocalFunBind ans $ G.unLoc bind
matchStmt ans (G.BodyStmt body _ _ _) = BodyStmt $ matchExprSI ans body
matchStmt ans (G.BindStmt pat body _ _ _) =
  BindStmt (error "BindStmt 1") (error "BindStmt 2")
matchStmt _ _ = error "unknown statement"

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

matchExprSI :: EP.Anns -> G.LHsExpr G.GhcPs -> Expr
matchExprSI ans = matchExpr ans <$> G.unLoc <*> getSrcInfo ans

matchExpr :: EP.Anns -> G.HsExpr G.GhcPs -> Maybe SrcInfo -> Expr
matchExpr ans (G.HsVar     name) = Var (getFunBindIdName ans name)
matchExpr ans (G.HsLit     lit ) = Lit (matchLit lit)
matchExpr ans (G.HsOverLit name) = OverLit (matchOverLit name)
matchExpr ans (G.HsApp expr1 expr2) =
  App (matchExprSI ans expr1)
      (matchExprSI ans expr2)
matchExpr ans (G.OpApp le op _ re) = OpApp (matchExprSI ans le)
                                       (matchExprSI ans op)
                                       (matchExprSI ans re)
matchExpr ans (G.HsIf _ ife thene elsee) = If (matchExprSI ans ife)
                                          (matchExprSI ans thene)
                                          (matchExprSI ans elsee)
matchExpr ans (G.HsLet locs expr) =
  Let (matchLocalFunBind ans $ G.unLoc locs) (matchExprSI ans expr)
matchExpr ans (G.ExprWithTySig expr typ) =
  ExprWithType (matchExprSI ans expr) (matchSigWcType ans typ)
matchExpr _ g = \_ -> error $ "unsupported " ++ ctorExprPrint g

matchLocalFunBind ans (G.HsValBinds (G.ValBindsIn binds sigs)) =
  let types = (matchSigBind ans . G.unLoc) <$> sigs
      vals  = (matchFunBind ans . G.unLoc) <$> G.bagToList binds
      --vals  = (matchFunBind <$> G.unLoc <*> getSrcInfo) <$> G.bagToList binds
  in  ValLocalBind vals types
matchLocalFunBind _ G.EmptyLocalBinds = EmptyLocalBind
matchLocalFunBind _ _                 = error "unsupported LocalBind ctor!"

--matchFunBind :: G.HsBindLR id G.GhcPs -> Decls
matchFunBind ans (G.FunBind id match _ _ _) = FunBind
  (getFunBindIdName ans id)
  (matchMatchGroup match)
  (getSrcInfo ans id)
 where
  matchMatchGroup (G.MG alts _ _ _) = (matchMatch <$> G.unLoc <*> getSrcInfo ans) <$> (G.unLoc alts)
  matchMatch (G.Match ctx pats grhs) =
    let argsp   = matchPatSI ans <$> pats
        exprss  = (getExprs ans . G.unLoc) <$> (dectrGRHSSExprs grhs)
        stmts   = (getStmts ans . G.unLoc) <$> (dectrGRHSSExprs grhs)
        localsb = getLocals $ G.unLoc (dectrGRHSSLocals grhs)
    in  Match argsp stmts exprss localsb
   where
    dectrGRHSSExprs (G.GRHSs bod loc) = bod
    dectrGRHSSLocals (G.GRHSs bod loc) = loc
    getExprs ans (G.GRHS stmts body) = matchExprSI ans body
    getStmts ans (G.GRHS stmts body) = getStmts' ans stmts

    getLocals = matchLocalFunBind ans

    getStmts' ans []     = [] --error "Empty"
    getStmts' ans (e:[]) = [matchStmt ans (G.unLoc e) (getSrcInfo ans e)]
    getStmts' ans (e:es) = error "I'm not the only one!"
matchFunBind _ _ = error "Other sig"

--matchSigWcType :: SOLONGTYPE -> Type
matchSigWcType ans (G.HsWC _ (G.HsIB _ t _)) = matchTypSI ans t

matchTypSI ans = matchTyp ans <$> G.unLoc <*> getSrcInfo ans

matchTyp :: EP.Anns -> G.HsType G.GhcPs -> Maybe SrcInfo -> Type
matchTyp ans (G.HsTyVar _ l) = TyVar $ getFunBindIdName ans l
matchTyp ans (G.HsAppsTy at) = TyApps $ appTypSI <$> at
 where
  appTypSI = apptyp <$> G.unLoc <*> getSrcInfo ans
  apptyp (G.HsAppInfix  l) = AppInfix $ getFunBindIdName ans l
  apptyp (G.HsAppPrefix t) = AppPrefix $ matchTypSI ans t

matchTyp ans (G.HsAppTy t1 t2) =
  TyApp (matchTypSI ans t1) (matchTypSI ans t2)
matchTyp ans (G.HsFunTy t1 t2) =
  TyFun (matchTypSI ans t1) (matchTypSI ans t2)
matchTyp ans (G.HsTupleTy _ tps) = error "Turple in types!"
matchTyp ans (G.HsListTy tp    ) = TyList $ matchTypSI ans tp
matchTyp _ _                   = error "unknown type"

matchSigBind ans (G.TypeSig id thing) = TypeSig (names id) (sigs thing)
 where
  names = fmap (getFunBindIdName ans)
  sigs (G.HsWC _ (G.HsIB _ b _)) = matchTypSI ans b
matchSigBind _ _ = error "unknown sig"

matchDecls :: EP.Anns -> G.HsDecl G.GhcPs -> Maybe SrcInfo -> Decls
matchDecls ans (G.ValD a) = ValDecl $ matchFunBind ans a
matchDecls ans (G.SigD a) = SigDecl $ matchSigBind ans a
matchDecls _ _          = error "unknown decl "

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
