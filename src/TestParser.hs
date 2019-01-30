{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module TestParser where

import qualified GHC as G
import qualified OccName as G
import qualified RdrName as G
import qualified BasicTypes as G
import qualified Bag as G

import Language.Haskell.GHC.ExactPrint.Parsers as Ps

import Data.List

import SimpleAST
import ASTParser
import OneLinePrinter

printFileOneLine file = do
  res <- Ps.parseModule file
  case res of
    Left (src, str) -> error str
    Right (ans, pds) -> return $ oneline $ getAst $ G.unLoc pds

expr file =
  do
    res <- Ps.parseModule file
    case res of
      Left (src, str) -> error str
      Right (ans, pds) -> return $ show $ getAst $ G.unLoc pds

{-
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


expr file = do
  res <- Ps.parseModule file
  case res of
    Left (src, str) -> error str
    Right (ans, pds) -> return $ show (matchDecls <$> (toDec $ G.hsmodDecls $ G.unLoc pds))
    --Right (ans, pds) -> return $ printMy <$> (matchDecls <$> (toDec $ G.hsmodDecls $ G.unLoc pds))
    --

printFile file = do
  res <- Ps.parseModule file
  case res of
    Left (src, str) -> error str
    Right (ans, pds) -> return $ unwords $ printDecl <$> (matchDecls <$> (toDec $ G.hsmodDecls $ G.unLoc pds))

toDec :: [G.LHsDecl a] -> [G.HsDecl a]
toDec = fmap G.unLoc


-- Old --

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
  let funBindName = concatMap (\s -> (handleFunBindId s) ++ " ")(G.unLoc <$> id)
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
      spats      = unwords $ (getInParameters . G.unLoc) <$> pats
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

getInParameters pat = case pat of
  G.VarPat id       -> handleFunBindId $ G.unLoc id
  G.ConPatIn id det -> (handleFunBindId $ G.unLoc id) ++ " " ++ (handleConPatDetail det)
  G.NPat lit mexpr synexpr _ -> handleOverLit $ G.unLoc lit
  G.ParPat pat -> "(" ++ (getInParameters $ G.unLoc pat) ++ ")"
  G.WildPat G.PlaceHolder -> "_"
  -- TODO potom
  _                 -> error "other pat"

handleConPatDetail (G.PrefixCon pat) = unwords $ (getInParameters . G.unLoc) <$> pat
handleConPatDetail (G.RecCon recf) = "RecCon"
handleConPatDetail (G.InfixCon pat1 pat2) = (getInParameters $ G.unLoc pat1) ++ " " ++ (getInParameters $ G.unLoc pat2)

-- Get Name of THE Function!
{-
handleFunBindId (G.Unqual name) = G.occNameString name
handleFunBindId (G.Qual mod name) =
  (G.moduleNameString mod) ++ "." ++ (G.occNameString name)
handleFunBindId (G.Orig mod name) = "Orig"
handleFunBindId (G.Exact name) = "Exact"
-}

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
handleExpr (G.HsCase expr match) = "case " ++ (handleExpr $ G.unLoc expr) ++ " of " ++ handleMatchGroup match
handleExpr (G.HsIf _ cond thn els) =
  "if "
    ++ (handleExpr $ G.unLoc cond)
    ++ " then "
    ++ (handleExpr $ G.unLoc thn)
    ++ " else "
    ++ (handleExpr $ G.unLoc els)
handleExpr (G.HsDo ctx exprs _) = "do {" ++ (concatMap (\s -> s ++ ";") $ (handleStmt . G.unLoc) <$> (G.unLoc exprs)) ++ "}"
handleExpr (G.ExplicitTuple tup _) = handleTuple $ G.unLoc <$> tup
handleExpr (G.ExplicitList _ _ exprs) = "[" ++ (handleList $ G.unLoc <$> exprs) ++ "]"
handleExpr c                       = error $ "Unsupported " ++ ctorExprPrint c

handleList [] = ""
handleList (e:[]) = handleExpr e
handleList (e:es) = (handleExpr e) ++ ", " ++ (handleList es)

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


-- handleStmt :: ExprLSmt p
--               LStmt p (LHsExpr p)
--               Located (StmtLR p p (LHsExpr p))
handleStmt (G.LetStmt bind) = ("let " ++ ) $ handleLocalBinds $ G.unLoc bind
handleStmt (G.BodyStmt body _ _ _) = handleExpr $ G.unLoc body
handleStmt (G.BindStmt pat body _ _ _) = (getInParameters $ G.unLoc pat) ++ " <- " ++ (handleExpr $ G.unLoc body)


getHs :: G.HsModule G.GhcPs -> Hs
getHs h =
  let name    = getModuleName h
      imports = getImports <$> G.unLoc <$> G.hsmodImports h
  in  Hs name imports decls


{-
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

-}

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

showHsModule :: G.HsModule G.GhcPs -> String
showHsModule hs = show $ getModuleName hs

showImportsModName :: G.HsModule G.GhcPs -> String
showImportsModName h =
  show
    $   G.moduleNameString
    <$> G.unLoc
    <$> G.ideclName
    <$> G.unLoc
    <$> G.hsmodImports h
-}
