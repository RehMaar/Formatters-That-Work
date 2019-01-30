module OneLinePrinter where

import Data.List

import SimpleAST

oneline :: Hs -> String
oneline (Hs name imps decls) = printModuleName name ++ printImports imps ++ (unwords $ printDecl <$> decls)

printModuleName Nothing = ""
printModuleName (Just n) = "module " ++ n ++ " where "

printImports = handleImports
  where
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



printDecl (ValDecl bind) = printBind bind
printDecl (SigDecl s) = printSig s

printBind (FunBind name matches) =
  intercalate "; " (printM <$> matches) ++ ";"
  where
    printM (Match args [[]] exprs locs) =
      name ++ " " ++ printArgs args ++ (ifStmts [] exprs) ++ printLocalBinds locs
    printM (Match args stmts exprs locs) =
      name ++ " " ++ printArgs args ++ (ifStmts (head <$> stmts) exprs) ++ printLocalBinds locs

    printArgs [] = ""
    printArgs (a:args) = printArg a ++ " " ++ printArgs args
    printArg (VarPat n) = n
    printArg (ParPat pat) = "( " ++ printArg pat ++ " )"
    printArg (NPat l) = printOverLit l
    printArg (ConstPat n det) = error "wtf"
    printArg WildPat = "_"

    ifStmts [] [] = ""
    ifStmts [] (ex:[]) = " = " ++ printExpr ex
    ifStmts (s:st) (e:ex) = " | " ++ (printStmt s) ++
                            " = " ++ (printExpr e) ++ ";" ++ ifStmts st ex

    printExpr (Var n) = n
    printExpr (OverLit l) = printOverLit l
    printExpr (Lit l) = printLit l
    printExpr (Lam m) = error "printExpr: lam"
    printExpr (App expr1 expr2) = (printExpr expr1) ++ " " ++ (printExpr expr2)
    printExpr (OpApp le op re) = (printExpr le) ++ " " ++ (printExpr op) ++ " " ++ (printExpr re)
    printExpr (Let locs expr) = "let " ++ (printLocalBinds locs) ++ " in " ++ (printExpr expr)
    printExpr (ExprWithType expr typ) = printExpr expr ++ " :: " ++ printType typ
    printExpr _ = "expr"

    printOverLit (OverLitInteger i) = show i
    printOverLit (OverLitFractional f) = show f
    printOverLit (OverLitString s) = show s

    printLit (LitChar c) = show c
    printLit (LitString s) = show s

    printLocalBinds EmptyLocalBind = ""
    printLocalBinds (ValLocalBind binds sigs) = " where " ++ unwords (printSig <$> sigs) ++ " " ++  unwords (printBind <$> binds)


    printStmt (BodyStmt expr) = printExpr expr
    printStmt _ = "stmt"

printBind OtherBind = error "OtherBind!"

printType (TyVar n) = n
printType (TyApps apt) = unwords $ printAppType <$> apt
  where
    printAppType (AppPrefix t) = printType t
    printAppType (AppInfix t) = t
printType (TyApp t1 t2) = (printType t1) ++ " " ++ (printType t2)
printType (TyFun t1 t2) = (printType t1) ++ " -> " ++ (printType t2)
printType (TyList t1) = "[" ++ (printType t1) ++ "]"

printSig (Type names tp) = (unwords names) ++ " :: " ++ (printType tp) ++ ";"
