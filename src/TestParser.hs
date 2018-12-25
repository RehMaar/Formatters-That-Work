{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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

deriving instance Show Decls
deriving instance Show Import
deriving instance Show Hs

an file = do
        res <- Ps.parseModule file
        case res of
            Left  (src, str) -> error str
            Right (ans, pds) -> return $ show $ ans

ps file = do
        res <- Ps.parseModule file
        case res of
            Left  (src, str) -> error str
            Right (ans, pds) -> return $ show $ handleSmth ans $ doSmth $ toDec $ G.hsmodDecls $ G.unLoc $ pds
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
isMatchCtor _ = False

--matchD (G.SigD a) = matchTypeSig a
--matchD (G.InstD a) = Inst
matchD (G.ValD  a) = matchBind a
--matchD (G.SigD  a) = Sig
matchD _ = undefined


--matchBind :: G.HsBindLR idL G.GhcPs -> String
matchBind (G.FunBind id match co fvs ticks) =
    let
        funBindName = handleFunBindId $ G.unLoc id
        matchStr = handleMatchGroup match
    in funBindName ++ " = " ++ matchStr

matchBind _ = error "other bind"

--handleMatchGroup :: G.MatchGroup p (G.GenLocated l (G.HsExpr G.GhcPs)) -> String
handleMatchGroup (G.MG alts argstys resty origin) = unwords $ mgAlts <$> G.unLoc <$> G.unLoc alts

--handleMatchGroup _ = error "other match group; huh?"

--mgAlts :: G.Match p (G.GenLocated l (G.HsExpr G.GhcPs)) -> String
mgAlts (G.Match ctx pats grhss) =
    let
        argh = getARGHHHGuard grhss
        localBinds = getARGHHHWhere grhss
    in (unwords $ argh) ++ " where " ++ (localBinds)
    where
        -- guard is guards and body
        -- body :: Located (HsExpr p)
        -- id :: Located (StmtLR p p (LHsExpr ps))
        grhsBody body = handleExpr body
        patGHRS (G.GRHS id body) = grhsBody $ G.unLoc body
        getARGHHHGuard g = patGHRS <$> G.unLoc <$> G.grhssGRHSs g
        getARGHHHWhere g = handleLocalBinds $ G.unLoc $ G.grhssLocalBinds g
        getInParameters pat =
            case G.unLoc pat of
                G.VarPat id -> handleFunBindId $ G.unLoc id
                -- TODO potom
                _           -> error "other pat"

-- Get Name of THE Function!
handleFunBindId (G.Unqual name) = G.occNameString name
handleFunBindId (G.Qual _ _) = "qual"

matchTypeSig (G.TypeSig locs hswt) = Tps.mkAnnKey <$> locs
matchTypeSig _ = undefined

handleExpr :: G.HsExpr G.GhcPs -> String
handleExpr (G.HsVar name) = handleFunBindId $ G.unLoc name
handleExpr (G.HsLet locbinds expr) =
     "let " ++
     "local: " ++ (handleLocalBinds $ G.unLoc locbinds) ++
     " in " ++ (handleExpr $ G.unLoc expr)
handleExpr (G.HsOverLit name) = "overlit: " ++ handleOverLit name
handleExpr (G.HsLit name) = "lit"
handleExpr (G.HsLam _) = "lam"
handleExpr (G.HsApp _ _) = "app"
handleExpr (G.OpApp le op _ re) = "opapp: " ++ (handleExpr $ G.unLoc le) ++ " "
                                            ++ (handleExpr $ G.unLoc op) ++ " "
                                            ++ (handleExpr $ G.unLoc re)
handleExpr (G.HsPar _) = "par"
handleExpr (G.HsCase _ _) = "case"
handleExpr (G.HsIf _ _ _ _) = "if"
handleExpr _ = "other"

handleOverLit :: G.HsOverLit G.GhcPs -> String
handleOverLit (G.OverLit val _ _ _) = handleOverLitVal val
    where
        handleOverLitVal (G.HsIntegral l) = handleInt l
        handleOverLitVal (G.HsFractional l) = handleFrac l
        handleOverLitVal (G.HsIsString t s) = FS.unpackFS s

        handleInt (G.IL text _ val) = show val
        handleFrac (G.FL text _ val) = show val

handleLocalBinds :: G.HsLocalBinds G.GhcPs -> String
handleLocalBinds (G.HsValBinds v) = handleValBinds v
handleLocalBinds (G.HsIPBinds v) = "implicit"
handleLocalBinds G.EmptyLocalBinds = "empty"

handleValBinds :: G.HsValBinds G.GhcPs -> String
handleValBinds (G.ValBindsIn bds sigs) =  valueBindings bds --"valbindsin"
    where
        valueBindings bds = unwords $ matchBind <$> G.unLoc <$> G.bagToList bds

handleValBinds (G.ValBindsOut _ _) = "valbindsout"

{-
\h -> fmap G.grhssGRHSs <$> fmap G.m_grhss
  <$> fmap G.unLoc <$> G.unLoc $ G.mg_alts
   $  patBind $ G.unLoc $ last $ decl2Bind h
-}


getHs :: G.HsModule G.GhcPs -> Hs
getHs h = let
            name = getModuleName h
            imports = getImports <$> G.unLoc <$> G.hsmodImports h
            decls = getDecls h
          in
          Hs name imports decls

getDecls :: G.HsModule G.GhcPs -> [Decls]
getDecls h = []

getD h = let decls = G.hsmodDecls h
         in (toBind decls, toSig decls)
    where
        toBind h = fmap patD <$> fmap G.unLoc <$> decl2Bind <$>h
        toSig h = fmap patS <$> fmap G.unLoc <$> decl2Sig <$> h

patBind (G.FunBind id match _ _ _) = match
patBind _ = error "not ok"

patS (G.TypeSig _ _) = TypeST
patS _ = OtherST

patD (G.FunBind _ _ _ _ _) = FuncBT
patD (G.PatBind _ _ _ _ _) = PattBT
patD (G.VarBind _ _ _    ) = VarBT
patD (G.AbsBinds _ _ _ _ _ _) = AbsBT
patD (G.PatSynBind _) = PatSynBT


oneline file = do
        res <- Ps.parseModule file
        case res of
            Left  (src, str) -> error str
            Right (ans, pds) -> return $ handleInfo $ G.unLoc $ pds
        where
            handleInfo :: G.HsModule G.GhcPs -> String
            handleInfo hm = let name = getModuleName hm
                                imports = importsFromMod hm
                            in
                            printModOneLine (Hs name imports [])

printModOneLine :: Hs -> String
printModOneLine hs = let
                        name = hsModuleName hs
                        imports = hsImports hs
                      in
                      handleName name ++ " " ++ handleImports imports
                      where
                          handleName (Nothing) = ""
                          handleName (Just modname) = "module " ++ modname ++ " where"

                          handleImports [] = ""
                          handleImports ((Import name qual as):imps) = "import " ++ handleImpQual qual ++ name ++ handleAs as ++ "; " ++ handleImports imps

                          handleImpQual True = " qualified "
                          handleImpQual False = ""

                          handleAs Nothing = ""
                          handleAs (Just alias) = " as " ++ alias

-- Module name
getModuleName :: G.HsModule G.GhcPs -> Maybe String
getModuleName h = G.moduleNameString <$> G.unLoc <$> G.hsmodName h

showHsModule :: G.HsModule G.GhcPs -> String
showHsModule hs = show $ getModuleName hs

-- Imports
getImports :: G.ImportDecl a -> Import
getImports h = let
                  name = G.moduleNameString $ G.unLoc $ G.ideclName h
                  isQuilified = G.ideclQualified h
                  alias = G.moduleNameString <$> G.unLoc <$> G.ideclAs h
               in
               Import name isQuilified alias

importsFromMod h = getImports <$> G.unLoc <$> G.hsmodImports h

showImportsModName :: G.HsModule G.GhcPs -> String
showImportsModName h = show $ G.moduleNameString  <$> G.unLoc <$> G.ideclName <$> G.unLoc <$> G.hsmodImports h

showDecls :: G.HsModule G.GhcPs -> String
showDecls h = undefined
