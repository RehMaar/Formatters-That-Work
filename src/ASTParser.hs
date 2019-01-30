{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module ASTParser where

import Language.Haskell.GHC.ExactPrint
import qualified GHC as G
import qualified OccName as G
import qualified RdrName as G
import qualified BasicTypes as G
import qualified Bag as G

import Language.Haskell.GHC.ExactPrint.Parsers as Ps
import Language.Haskell.GHC.ExactPrint.Pretty as Pt
import Language.Haskell.GHC.ExactPrint.Types as Tps

import qualified Data.Map.Strict as Map
import qualified FastString as FS

import Data.List

import SimpleAST

parseExpr file =
  do
    res <- Ps.parseModule file
    case res of
      Left (src, str) -> error str
      Right (ans, pds) -> return $ show $ getAst $ G.unLoc pds

getAst :: G.HsModule G.GhcPs -> Hs
getAst = Hs <$> getModuleName <*> importsFromMod <*> getDecls

getDecls :: G.HsModule G.GhcPs -> [Decls]
getDecls hm = matchDecls <$> toDec (G.hsmodDecls hm)
  where
    toDec :: [G.LHsDecl a] -> [G.HsDecl a]
    toDec = fmap G.unLoc

-- Module name
getModuleName :: G.HsModule G.GhcPs -> Maybe String
getModuleName h = G.moduleNameString <$> G.unLoc <$> G.hsmodName h

-- Imports
getImports :: G.ImportDecl a -> Import
getImports h =
  let name        = G.moduleNameString $ G.unLoc $ G.ideclName h
      isQuilified = G.ideclQualified h
      alias       = G.moduleNameString <$> G.unLoc <$> G.ideclAs h
  in  Import name isQuilified alias

importsFromMod :: G.HsModule name -> [Import]
importsFromMod h = getImports <$> G.unLoc <$> G.hsmodImports h

matchPat :: G.Pat G.GhcPs -> Pat
matchPat (G.WildPat G.PlaceHolder) = WildPat
matchPat (G.VarPat name) = VarPat $ handleFunBindId $ G.unLoc name
matchPat (G.ConPatIn name det) = ConstPat (handleFunBindId $ G.unLoc name) (matchDetails det)
  where
    -- TODO: wtf
    matchDetails det = PrefixConPat []
matchPat (G.NPat lit _ _ _) = NPat $ matchOverLit $ G.unLoc lit
matchPat (G.ParPat pat) = ParPat $ error "ParPat"
matchPat _ = error "Other Pattern"

-- ParStmt -- for list compreh
matchStmt :: G.ExprStmt G.GhcPs -> Stmt
matchStmt (G.LetStmt bind) = LetStmt $ matchLocalFunBind $ G.unLoc bind
matchStmt (G.BodyStmt body _ _ _) = BodyStmt $ matchExpr $ G.unLoc body
matchStmt (G.BindStmt pat body _ _ _ ) = BindStmt (error "BindStmt 1") (error "BindStmt 2")
matchStmt _ = error "unknown statement"

matchLit :: G.HsLit x -> Literals
matchLit (G.HsChar _ c) = LitChar c
matchLit (G.HsString _ s) = LitString $ FS.unpackFS s
matchLit _ = error "unsupported literal type"

matchOverLit :: G.HsOverLit G.GhcPs -> OverLiterals
matchOverLit (G.OverLit val _ _ _) = handleOL val
  where
      handleOL (G.HsIntegral l) = OverLitInteger $ handleInt l
      handleOL (G.HsFractional l) = OverLitFractional $ handleFrac l
      handleOL (G.HsIsString t s) = OverLitString $ FS.unpackFS s

      handleInt (G.IL text _ val) = val
      handleFrac (G.FL text _ val) = val

matchExpr :: G.HsExpr G.GhcPs -> Expr
matchExpr (G.HsVar name)             = Var $ handleFunBindId $ G.unLoc name
matchExpr (G.HsLit lit)              = Lit $ matchLit lit
matchExpr (G.HsOverLit name)         = OverLit $ matchOverLit name
matchExpr (G.HsApp expr1 expr2)      = App (matchExpr $ G.unLoc expr1) (matchExpr $ G.unLoc expr2)
matchExpr (G.OpApp le op _ re)       = OpApp (matchExpr $ G.unLoc le) (matchExpr $ G.unLoc op) (matchExpr $ G.unLoc re)
matchExpr (G.HsIf _ ife thene elsee) = If (matchExpr $ G.unLoc ife) (matchExpr $ G.unLoc thene) (matchExpr $ G.unLoc elsee)
matchExpr (G.HsLet locs expr)        = Let (matchLocalFunBind $ G.unLoc locs) (matchExpr $ G.unLoc expr)
matchExpr (G.ExprWithTySig expr typ) = ExprWithType (matchExpr $ G.unLoc expr) (matchSigWcType typ)
matchExpr g = error $ "unsupported " ++ ctorExprPrint g

--matchSigWcType :: SOLONGTYPE -> Type
matchSigWcType (G.HsWC _ (G.HsIB _ t _)) = matchTyp $ G.unLoc t

matchLocalFunBind (G.HsValBinds (G.ValBindsIn binds sigs)) =
  let types = (matchSigBind . G.unLoc) <$> sigs
      vals  = (matchFunBind . G.unLoc) <$> G.bagToList binds
  in ValLocalBind vals types
  where
      valBinds binds = undefined

matchLocalFunBind G.EmptyLocalBinds = EmptyLocalBind
matchLocalFunBind _ = error "unsupported LocalBind ctor!"

--matchFunBind :: G.HsBindLR id G.GhcPs -> Decls
matchFunBind (G.FunBind id match _ _ _) = FunBind (handleFunBindId $ G.unLoc id) (matchMatchGroup match)
  where
      matchMatchGroup (G.MG alts _ _ _) = (matchMatch . G.unLoc) <$> (G.unLoc alts)
      matchMatch (G.Match ctx pats grhs) =
        let argsp = (matchPat . G.unLoc) <$> pats
            exprss = (getExprs . G.unLoc) <$> (dectrGRHSSExprs grhs)
            stmts = (getStmts . G.unLoc) <$> (dectrGRHSSExprs grhs)
            localsb = getLocals $ G.unLoc (dectrGRHSSLocals grhs)
        in Match argsp stmts exprss localsb
        where
          dectrGRHSSExprs (G.GRHSs bod loc) = bod
          dectrGRHSSLocals (G.GRHSs bod loc) = loc
          getExprs (G.GRHS stmts body) = matchExpr $ G.unLoc body
          getStmts (G.GRHS stmts body) = getStmts' stmts

          getLocals = matchLocalFunBind

          getStmts' [] = [] --error "Empty"
          getStmts' (e:[]) = [matchStmt $ G.unLoc e]
          getStmts' (e:es) = error "Not only one!"
matchFunBind _ = error "Other sig"


matchTyp :: G.HsType G.GhcPs -> Type
matchTyp (G.HsTyVar _ l) = TyVar $ handleFunBindId $ G.unLoc l
matchTyp (G.HsAppsTy at) = TyApps $ (apptyp . G.unLoc) <$> at
  where
    apptyp (G.HsAppInfix l) = AppInfix $ handleFunBindId $ G.unLoc l
    apptyp (G.HsAppPrefix t) = AppPrefix $ matchTyp $ G.unLoc t
matchTyp (G.HsAppTy t1 t2) = TyApp (matchTyp $ G.unLoc t1) (matchTyp $ G.unLoc t2)
matchTyp (G.HsFunTy t1 t2) = TyFun (matchTyp $ G.unLoc t1) (matchTyp $ G.unLoc t2)
matchTyp (G.HsTupleTy _ tps) = error "Turple in types!"
matchTyp (G.HsListTy tp) = TyList $ matchTyp $ G.unLoc tp
matchTyp _ = error "unknown type"

matchSigBind (G.TypeSig id thing)  = Type (names id) (sigs thing)
  where
    names = fmap (handleFunBindId . G.unLoc)
    sigs (G.HsWC _ (G.HsIB _ b _)) = matchTyp $ G.unLoc b
matchSigBind _ = error "unknown sig"

matchDecls :: G.HsDecl G.GhcPs -> Decls
matchDecls (G.ValD a) = ValDecl $ matchFunBind a
matchDecls (G.SigD a) = SigDecl $ matchSigBind a
matchDecls  _ = error "unknown decl "

handleFunBindId (G.Unqual name) = G.occNameString name
handleFunBindId (G.Qual mod name) =
  (G.moduleNameString mod) ++ "." ++ (G.occNameString name)
handleFunBindId (G.Orig mod name) = error "unknown name of type `Orig'"
handleFunBindId (G.Exact name) = error "unknown name of type `Exac't"

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
