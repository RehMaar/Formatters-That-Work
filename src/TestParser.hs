{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Language.Haskell.GHC.ExactPrint
import qualified GHC as G

import Language.Haskell.GHC.ExactPrint.Parsers as Ps
import Language.Haskell.GHC.ExactPrint.Pretty as Pt


data Import = Import {
    iName :: String,
    iQuilified :: Bool,
    iAs :: Maybe String
}

data Bind = Not | Func | Pat | Var | Abs | PatSyn deriving Show
data Sig = Other | Type deriving Show

data DeclType = Bind | Sig deriving Show

data Decls = Decls {
    declType :: DeclType
}

data Hs = Hs {
    hsModuleName :: Maybe String,
    hsImports :: [Import],
    hsDecls   :: [Decls]
}

deriving instance Show Decls
deriving instance Show Import
deriving instance Show Hs

f file = do
        res <- Ps.parseModule file
        case res of
            Left  (src, str) -> error str
            Right (ans, pds) -> return $ show $ getD $ G.unLoc $ pds

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

{-

\h -> fmap G.grhssGRHSs <$> fmap G.m_grhss <$> fmap G.unLoc <$> G.unLoc $ G.mg_alts $ patBind $ G.unLoc $ last $ decl2Bind h
  :: LHsDecl p -> [[LGRHS p (LHsExpr p)]]

-}

patS (G.TypeSig _ _) = Type
patS _ = Other

patD (G.FunBind _ _ _ _ _) = Func
patD (G.PatBind _ _ _ _ _) = Pat
patD (G.VarBind _ _ _    ) = Var
patD (G.AbsBinds _ _ _ _ _ _) = Abs
patD (G.PatSynBind _) = PatSyn


getModuleName :: G.HsModule G.GhcPs -> Maybe String
getModuleName h = G.moduleNameString <$> G.unLoc <$> G.hsmodName h

getImports :: G.ImportDecl a -> Import
getImports h = let
                  name = G.moduleNameString $ G.unLoc $ G.ideclName h
                  isQuilified = G.ideclQualified h
                  alias = G.moduleNameString <$> G.unLoc <$> G.ideclAs h
               in
               Import name isQuilified alias

showHsModule :: G.HsModule G.GhcPs -> String
showHsModule hs = show $ getModuleName hs

showImportsModName :: G.HsModule G.GhcPs -> String
showImportsModName h = show $ G.moduleNameString  <$> G.unLoc <$> G.ideclName <$> G.unLoc <$> G.hsmodImports h

showDecls :: G.HsModule G.GhcPs -> String
showDecls h = undefined
