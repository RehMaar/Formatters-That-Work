{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}


module TestParser where


import Language.Haskell.GHC.ExactPrint
import qualified GHC as G
import qualified OccName as G
import qualified BasicTypes as G
import qualified Bag as G

import Language.Haskell.GHC.ExactPrint.Parsers as Ps
import Language.Haskell.GHC.ExactPrint.Pretty as Pt
import Language.Haskell.GHC.ExactPrint.Types as Tps

import qualified Data.Map.Strict as Map
import qualified FastString as FS

import Data.List

data Import = Import {
    iName :: String,
    iQuilified :: Bool,
    iAs :: Maybe String
}

data SigType = OtherST | TypeST deriving Show

data BindType = NotBT | FuncBT | PattBT | VarBT | AbsBT | PatSynBT deriving Show

data PatType = ParPT | ListPT | TuplePT | OthPT deriving Show


--{-

data Pat = PatVar String | Other PatType

data Match = Match Pat Expr

data Bind = FunBind {
        fun_name :: String,
        fun_matches :: Match
    }
  | OtherBind BindType


data Expr = Var
          | OverLit String
          | Lit String
          | Lam Match
          | App Match Match
          | OpApp Expr Expr Expr
          | Let Bind Expr
---}

data DeclType = BindDT | SigDT | TyClDT | InstDT deriving Show

data Decls = ValDecls Bind | OtherDecls DeclType

data Hs = Hs {
    hsModuleName :: Maybe String,
    hsImports :: [Import],
    hsDecls   :: [Decls]
}

deriving instance Show Expr
deriving instance Show Pat
deriving instance Show Match
deriving instance Show Bind
deriving instance Show Decls
deriving instance Show Import
deriving instance Show Hs

an file = do
  res <- Ps.parseModule file
  case res of
    Left  (src, str) -> error str
    Right (ans, pds) -> return $ show $ headerComments $ ans

headerComments = handleAnn . snd . head . Map.toList
 where
  handleAnn :: Annotation -> String
  handleAnn (Ann _ _ _ d _ _) = concatMap (hd . fst) d
   where
    hd (AnnComment cmt) = cmnt cmt
    hd _                = ""

    cmnt (Comment cont id orig) =
      if "--" `isPrefixOf` cont then "{-" ++ tail (tail cont) ++ "-}" else cont

ps file = do
  res <- Ps.parseModule file
  case res of
    Left (src, str) -> error str
    Right (ans, pds) ->
      return
        $ show
        $ handleSmth ans
        $ doSmth
        $ toDec
        $ G.hsmodDecls
        $ G.unLoc
        $ pds

toDec :: [G.LHsDecl a] -> [G.HsDecl a]
toDec = fmap G.unLoc

--handleSmth :: Maybe [Tps.AnnKey] -> Maybe [Tps.Annotation]
--handleSmth a Nothing = Nothing
--handleSmth a (Just keys) = Just $ (\k -> Map.lookup k a) keys
handleSmth _ = id

--doSmth :: [G.HsDecl a] ->
--doSmth [] = Nothing
--doSmth (d:ds) = if isMatchCtor d then matchD d else doSmth ds
doSmth = fmap matchD . filter isMatchCtor

isMatchCtor (G.ValD _) = True
isMatchCtor (G.SigD _) = True
isMatchCtor _          = False

--matchD (G.InstD a) = Inst
matchD (G.ValD a) = matchBind a
matchD (G.SigD a) = matchTypeSig a
matchD _          = undefined


matchTypeSig :: G.Sig G.GhcPs -> String
matchTypeSig (G.TypeSig id thing) =
  let funBindName = concatMap handleFunBindId (G.unLoc <$> id)
      matchType   = handleHWCB thing
  in  funBindName ++ " :: " ++ matchType ++ ";"
matchTypeSig _ = "type: unknown"

--handleHWCB :: G.HsWildCardBndrs id G.GhcPs   -> String
handleHWCB (G.HsWC _ body) = handleImBdrs body

--handleImBdrs :: G.HsImplicitBndrs G.GhcPs id -> String
handleImBdrs (G.HsIB _ body _) = handleType $ G.unLoc body

--handleType (G.HsQualTy _ _ ) = "HsQualTy"
handleType (G.HsTyVar _ l) = handleFunBindId $ G.unLoc l
handleType (G.HsAppsTy at) = unwords $ (handleAppTy . G.unLoc) <$> at
handleType (G.HsAppTy t1 t2) =
  (handleType $ G.unLoc t1) ++ " " ++ (handleType $ G.unLoc t1)
handleType (G.HsFunTy t1 t2) =
  (handleType $ G.unLoc t1) ++ " -> " ++ (handleType $ G.unLoc t1)
--handleType (G.HsListTy _) = "HsListTy"
handleType (G.HsTupleTy _ tps) = "(" ++ handleTupleTy tps ++ ")"
--handleType (G.HsParTy _) = "HsParTy"
handleType _                   = error "Unsupported sig type"

handleAppTy (G.HsAppInfix  l) = handleFunBindId $ G.unLoc l
handleAppTy (G.HsAppPrefix t) = handleType $ G.unLoc t

handleTupleTy []     = ""
handleTupleTy (t:[]) = handleType $ G.unLoc t
handleTupleTy (t:ts) = (handleType $ G.unLoc t) ++ "," ++ handleTupleTy ts

--matchBind :: G.HsBindLR idL G.GhcPs -> String
matchBind (G.FunBind id match co fvs ticks) =
  let funBindName = handleFunBindId $ G.unLoc id
      matchStr    = handleMatchGroup match
  in  funBindName ++ " " ++ matchStr

matchBind _ = error "other bind"

handleMatchGroup :: G.MatchGroup G.GhcPs (G.LHsExpr G.GhcPs) -> String
handleMatchGroup (G.MG alts _ _ origin) =
  unwords $ (mgAlts . G.unLoc) <$> G.unLoc alts

--handleMatchGroup _ = error "other match group; huh?"

--mgAlts :: G.Match p (G.GenLocated l (G.HsExpr G.GhcPs)) -> String
mgAlts (G.Match ctx pats grhss) =
  let argh       = getARGHHHGuard grhss
      localBinds = getARGHHHWhere grhss
      spats      = unwords $ getInParameters <$> pats
      body =
        " = "
          ++ (unwords $ argh)
          ++ (if null localBinds then "" else " where " ++ (localBinds))
  in  spats ++ body ++ ";"
 where
        -- guard is guards and body
        -- body :: Located (HsExpr p)
        -- id :: Located (StmtLR p p (LHsExpr ps))
  grhsBody body = handleExpr body
  patGHRS (G.GRHS id body) = grhsBody $ G.unLoc body
  getARGHHHGuard g = patGHRS <$> G.unLoc <$> G.grhssGRHSs g
  getARGHHHWhere g = handleLocalBinds $ G.unLoc $ G.grhssLocalBinds g
  getInParameters pat = case G.unLoc pat of
    G.VarPat id       -> handleFunBindId $ G.unLoc id
    G.ConPatIn id det -> "ConPatIn"
    -- TODO potom
    _                 -> error "other pat"

-- Get Name of THE Function!
handleFunBindId (G.Unqual name) = G.occNameString name
handleFunBindId (G.Qual mod name) =
  (G.moduleNameString mod) ++ "." ++ (G.occNameString name)

handleExpr :: G.HsExpr G.GhcPs -> String
handleExpr (G.HsVar name) = (handleFunBindId $ G.unLoc name)
handleExpr (G.HsLet locbinds expr) =
  "let "
    ++ (handleLocalBinds $ G.unLoc locbinds)
    ++ " in "
    ++ (handleExpr $ G.unLoc expr)
handleExpr (G.HsOverLit name) = handleOverLit name
handleExpr (G.HsLit     name) = handleLit name
handleExpr (G.HsLam     _   ) = "lam"
handleExpr (G.HsApp expr1 expr2) =
  (handleExpr $ G.unLoc expr1) ++ " (" ++ (handleExpr $ G.unLoc expr2) ++ ")" -- for operators like (+)
handleExpr (G.OpApp le op _ re) =
  (handleExpr $ G.unLoc le)
    ++ " "
    ++ (handleExpr $ G.unLoc op)
    ++ " "
    ++ (handleExpr $ G.unLoc re)
handleExpr (G.HsPar expr) = "(" ++ (handleExpr $ G.unLoc expr) ++ ")"
--handleExpr (G.HsCase expr match) = "case " ++ (handleExpr $ G.unLoc expr) ++ " of " ++ handleMatchGroup match
handleExpr (G.HsIf _ cond thn els) =
  "if "
    ++ (handleExpr $ G.unLoc cond)
    ++ " then "
    ++ (handleExpr $ G.unLoc thn)
    ++ " else "
    ++ (handleExpr $ G.unLoc els)
handleExpr (G.ExplicitTuple tup _) = handleTuple $ G.unLoc <$> tup
handleExpr c                       = error $ "Unsupported " ++ ctorExprPrint c

handleLit (G.HsChar   _ c) = show c
handleLit (G.HsString _ c) = show c
handleLit _                = error "unsupported litterals"

handleOverLit :: G.HsOverLit G.GhcPs -> String
handleOverLit (G.OverLit val _ _ _) = handleOverLitVal val
 where
  handleOverLitVal (G.HsIntegral   l) = handleInt l
  handleOverLitVal (G.HsFractional l) = handleFrac l
  handleOverLitVal (G.HsIsString t s) = FS.unpackFS s

  handleInt (G.IL text _ val) = show val
  handleFrac (G.FL text _ val) = show val

handleLocalBinds :: G.HsLocalBinds G.GhcPs -> String
handleLocalBinds (G.HsValBinds v)  = handleValBinds v
handleLocalBinds (G.HsIPBinds  v)  = "implicit"
handleLocalBinds G.EmptyLocalBinds = ""

handleValBinds :: G.HsValBinds G.GhcPs -> String
handleValBinds (G.ValBindsIn bds sigs) =
  let types = typeBindings sigs
      vals  = valueBindings bds
  in  if null types
        then unwords vals
        else concatMap (\(e, t) -> t ++ " " ++ e)
          $ zip (valueBindings bds) (typeBindings sigs)
 where
  valueBindings bds = matchBind <$> G.unLoc <$> G.bagToList bds
  typeBindings sigs = matchTypeSig <$> G.unLoc <$> sigs

handleValBinds (G.ValBindsOut _ _) = "valbindsout"

handleTuple ts = "(" ++ concatMap handleTuple' ts ++ ")"
 where
  handleTuple' (G.Present expr) = handleExpr $ G.unLoc expr
  handleTuple' (G.Missing _   ) = ","


getHs :: G.HsModule G.GhcPs -> Hs
getHs h =
  let name    = getModuleName h
      imports = getImports <$> G.unLoc <$> G.hsmodImports h
      decls   = getDecls h
  in  Hs name imports decls

getDecls :: G.HsModule G.GhcPs -> [Decls]
getDecls h = []

getD h = let decls = G.hsmodDecls h in (toBind decls, toSig decls)
 where
  toBind h = fmap patD <$> fmap G.unLoc <$> decl2Bind <$> h
  toSig h = fmap patS <$> fmap G.unLoc <$> decl2Sig <$> h

patBind (G.FunBind id match _ _ _) = match
patBind _                          = error "not ok"

patS (G.TypeSig _ _) = TypeST
patS _               = OtherST

patD (G.FunBind _ _ _ _ _   ) = FuncBT
patD (G.PatBind _ _ _ _ _   ) = PattBT
patD (G.VarBind _ _ _       ) = VarBT
patD (G.AbsBinds _ _ _ _ _ _) = AbsBT
patD (G.PatSynBind _        ) = PatSynBT


oneline file = do
  res <- Ps.parseModule file
  case res of
    Left (src, str) -> error str
    Right (ans, pds) ->
      return $ headerComments ans ++ " " ++ myAst pds ++ " " ++ hsAst pds
 where
  myAst = handleInfo . G.unLoc
  hsAst =
    (foldr (\x y -> x ++ ";" ++ y) "") . doSmth . toDec . G.hsmodDecls . G.unLoc

  handleInfo :: G.HsModule G.GhcPs -> String
  handleInfo hm =
    let name    = getModuleName hm
        imports = importsFromMod hm
    in  printModOneLine (Hs name imports [])

printModOneLine :: Hs -> String
printModOneLine hs =
  let name    = hsModuleName hs
      imports = hsImports hs
  in  handleName name ++ " " ++ handleImports imports
 where
  handleName (Nothing     ) = ""
  handleName (Just modname) = "module " ++ modname ++ " where"

  handleImports [] = ""
  handleImports ((Import name qual as):imps) =
    "import "
      ++ handleImpQual qual
      ++ name
      ++ handleAs as
      ++ "; "
      ++ handleImports imps

  handleImpQual True  = " qualified "
  handleImpQual False = ""

  handleAs Nothing      = ""
  handleAs (Just alias) = " as " ++ alias

-- Module name
getModuleName :: G.HsModule G.GhcPs -> Maybe String
getModuleName h = G.moduleNameString <$> G.unLoc <$> G.hsmodName h

showHsModule :: G.HsModule G.GhcPs -> String
showHsModule hs = show $ getModuleName hs

-- Imports
getImports :: G.ImportDecl a -> Import
getImports h =
  let name        = G.moduleNameString $ G.unLoc $ G.ideclName h
      isQuilified = G.ideclQualified h
      alias       = G.moduleNameString <$> G.unLoc <$> G.ideclAs h
  in  Import name isQuilified alias

importsFromMod h = getImports <$> G.unLoc <$> G.hsmodImports h

showImportsModName :: G.HsModule G.GhcPs -> String
showImportsModName h =
  show
    $   G.moduleNameString
    <$> G.unLoc
    <$> G.ideclName
    <$> G.unLoc
    <$> G.hsmodImports h

showDecls :: G.HsModule G.GhcPs -> String
showDecls h = undefined



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
