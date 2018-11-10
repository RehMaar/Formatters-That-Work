{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving     #-}
{-# LANGUAGE TypeOperators, PatternSynonyms           #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts      #-}

module HsSyn where

type Name = String

data FnType a = Fn a | Op a

--data Imports = Import Name (Maybe Name) (Maybe (Bool, [Names]))
data Import = Import {
    name :: Name,
    isQualified :: Bool,
    shortcut :: Maybe Name,
    importedSymbols :: Maybe [Name] -- TODO: [FnType Name]
} deriving Show

type Decls = Int

{-

[ module NAME [ names ] where ]

import [quilified] NAME [as SHORTNAME] (hiding) [(names)]

Что такое экспорты?

-}
data HsMod = HsMod {
    -- Module name
    hsModuleName :: Maybe Name,
    -- Exported symbols
    hsExports :: [Name],
    -- Imports
    hsImports :: [Import],

    hsDecl :: [Decls]
}
