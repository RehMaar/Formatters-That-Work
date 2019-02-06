module OneLinePrinter (oneline) where

import Data.List

import AnnTtgAST

import CommonASTParser as CAP
import AnnConverter

import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Types as EP
import qualified ApiAnnotation as EP

olExpr file = do
  res <- EP.parseModule file
  case res of
    Left  (src, str) -> error str
    Right (ans, pds) -> return $ oneline $ toAnn $ CAP.getAst ans pds

oneline :: Hs -> String
oneline (Hs name imps decls si) =
  printModule (getAnn si) name
    ++ printImports imps
    ++ unwords (printDecl <$> decls)
    ++ eofAnn (getAnn si)
 where
  eofAnn Nothing    = ""
  eofAnn (Just ann) = case findComment ann (EP.G EP.AnnEofPos) of
    Nothing      -> ""
    Just comment -> " " ++ printComment comment

printComment (EP.Comment str _ _) =
  if "--" `isPrefixOf` str then "{- " ++ str ++ " -}" else str
printComment' Nothing                       n = n
printComment' (Just (EP.Ann _ pc fc _ _ _)) n = p' pc ++ n ++ f' fc
 where
  f' fc =
    let fs' = printOutComment fc in if (not . null) fs' then " " ++ fs' else ""
  p' pc =
    let ps' = printOutComment pc in if (not . null) ps' then ps' ++ " " else ""


printOutComment c = unwords ((printComment . fst) <$> c)

printName (Name n si) = case getAnn si of
  Nothing  -> n
  Just ann -> handleNameAnn ann n
 where
  handleNameAnn (EP.Ann _ pc fc ki _ _) n =
    printOutComment pc ++ printC ki ++ n ++ printOutComment fc
  printC []                        = ""
  printC ((EP.AnnComment c, _):es) = printComment c ++ printC es
  printC (_                   :es) = printC es


hasAnn :: EP.Annotation -> EP.KeywordId -> Bool
hasAnn (EP.Ann _ _ _ ki _ _) key = find ki
 where
  find []          = False
  find ((k, _):es) = (k == key) || find es

findComment :: EP.Annotation -> EP.KeywordId -> Maybe EP.Comment
findComment (EP.Ann _ _ _ ki _ _) key = find ki
 where
  find [] = Nothing
  find ((EP.AnnComment c, _):(key', _):es) =
    if key' == key then Just c else find es
  find (_:es) = find es

getAnn Nothing              = Nothing
getAnn (Just (SrcInfo ann)) = ann

printModule (Just ann) Nothing = ""
printModule ann (Just name) =
  moduleAnn ann ++ nameAnn ann ++ printName name ++ whereAnn ann
 where
  moduleAnn Nothing    = "module "
  moduleAnn (Just ann) = case findComment ann (EP.G EP.AnnModule) of
    Nothing      -> "module "
    Just comment -> printComment comment ++ " module "

  nameAnn Nothing = ""
  nameAnn (Just ann) =
    case findComment ann (EP.G EP.AnnVal) of
      Nothing      -> ""
      Just comment -> printComment comment ++ " "

  whereAnn Nothing    = " where "
  whereAnn (Just ann) = case findComment ann (EP.G EP.AnnWhere) of
    Nothing      -> " where "
    Just comment -> printComment comment ++ " where "

printImports []         = ""
printImports (imp:imps) = printImport imp ++ "; " ++ printImports imps
 where
  printImport (Import name qual as si) =
    importAnn ann ++ qualAnn ann ++ printName name ++ asAnn ann ++ printAs as
   where
    ann = getAnn si
    importAnn Nothing = "import "
    importAnn (Just (EP.Ann _ pc fc ki _ _)) =
      let s = printOutComment pc
      in  if (not . null) s then s ++ " import " else "import "

    qualAnn Nothing  = if qual then "qualified " else ""
    qualAnn (Just a) = if not qual
      then ""
      else case findComment a (EP.G EP.AnnQualified) of
        Nothing -> "qualified "
        Just c  -> printComment c ++ " qualified "

    asAnn Nothing  = ""
    asAnn (Just a) = case findComment a (EP.G EP.AnnAs) of
      Nothing -> ""
      Just c  -> " " ++ printComment c

    printAs Nothing     = ""
    printAs (Just name) = " as " ++ printName name

printDecl (ValDecl bind si) = printComment' (getAnn si) (printBind bind) ++ ";"
printDecl (SigDecl s    si) = printSig (getAnn si) s

printBind (FunBind name matches sif) = {-"{ " ++ show (getAnn sif) ++ " }" ++ -}intercalate ";" (printMatch <$> matches)
 where
    {-
       name m_args | m_stmts!!0 = exprs!!0
                   | m_stmts!!1 = exprs!!1
                   ...
                   | m_stmts!!n = expts!!n
           where
             locals
    -}
  printMatch (Match args grhss locs si) =
    let s =
          printName name
            ++ " "
            ++ printPats args
            ++ match (getAnn si)
            ++ stmts
            ++ printWhere locs
        stmts = intercalate ";" $ printG <$> grhss
        match Nothing  = ""
        match (Just a) = case findComment a (EP.G EP.AnnEqual) of
          Nothing -> ""
          Just c  -> printComment c ++ " "
    in  printComment' (getAnn si) s

  printPats = unwords . fmap printPat

  printPat (VarPat name si) =
    printComment' (getAnn si) (printName name ++ " ")
  printPat (ParPat pat si) = {- "{PP " ++ show si ++ " PP}" -}"(" ++ printPat pat ++ ")"
  printPat (NPat lit si) = printComment' (getAnn si) (printOverLit lit)
  printPat (ConstPat name cpd si) = {- "{CP " ++ show si ++ " CP}" -}undefined
  printPat (WildPat si) = {- "{WP " ++ show si ++ " WP}" -}" _ "

  printG (GRHS stmt expr si) = printG' stmt expr (getAnn si)
  printG' []     expr _       = "= " ++ printExpr expr
  printG' [stmt] expr Nothing = " | " ++ printStmt stmt ++ printExpr expr
  printG' [stmt] expr (Just ann@(EP.Ann _ pc fc ki _ _)) =
    printOutComment pc
      ++ vbarAnn
      ++ stmtAnn stmt
      ++ equalAnn
      ++ printExpr expr
      ++ printOutComment fc
   where
    vbarAnn = case findComment ann (EP.G EP.AnnVbar) of
      Nothing      -> " | "
      Just comment -> printComment comment ++ " | "

    stmtAnn  = printStmt

    equalAnn = case findComment ann (EP.G EP.AnnEqual) of
      Nothing      -> " = "
      Just comment -> printComment comment ++ " = "

  printL [s] e = " | " ++ printStmt s ++ " = " ++ printExpr e

  printStmt (BindStmt pat expr si) =
    printComment' (getAnn si) (error "not impl")
  printStmt (LetStmt  lb   si) = printComment' (getAnn si) (error "not impl")
  printStmt (BodyStmt expr si) = printComment' (getAnn si) (printExpr expr)

  printWhere EmptyLocalBind = ""
  -- TODO: Match bindsd ans sigs
  printWhere (ValLocalBind bs ss) =
    " where "
      ++ unwords (printBind <$> bs)
      ++ " "
      ++ unwords (printSig Nothing <$> ss)

  printOverLit (OverLitInteger    i) = show i
  printOverLit (OverLitFractional f) = show f
  printOverLit (OverLitString     s) = show s

  printLit (LitChar   c) = show c
  printLit (LitString s) = show s

  printExpr (Var name@(Name _ sin) si) = printVar (getAnn sin) (printName name)
    where
      pritnVar Nothing n = n
      printVar (Just ann@(EP.Ann _ _ _ ki _ _)) n = openB ++ n ++ closeB
        where
          openB = if hasAnn ann (EP.G EP.AnnOpenP) then "(" else ""

          closeB = if hasAnn ann (EP.G EP.AnnCloseP) then printCloseComm ++ ")" else ""

          printCloseComm = case findComment ann (EP.G EP.AnnCloseP) of {Nothing -> ""; Just c -> " " ++ printComment c ++ " "}

  printExpr (OverLit l si      ) = printComment' (getAnn si) (printOverLit l)
  printExpr (Lit     l si      ) = printComment' (getAnn si) (printLit l)
  printExpr (Lam     m si      ) = error "not impl"
  printExpr (App expr1 expr2 si) = printExpr expr1 ++ " " ++ printExpr expr2
  printExpr (OpApp le op re si) =
    printComment' (getAnn si)
      $  printExpr le
      ++ " "
      ++ printExpr op
      ++ " "
      ++ printExpr re
  printExpr (Let locs expr si) = ""
  printExpr (If c t e si) =
    "if " ++ printExpr c ++ " then " ++ printExpr t ++ " else " ++ printExpr e
  printExpr (ExprWithType expr typ si) =
    printExpr expr ++ " :: " ++ printType typ
  printExpr (Par expr si) = "(" ++ printExpr expr ++ printCloseAnn (getAnn si) ++ ")"
    where
      printCloseAnn Nothing    = ""
      printCloseAnn (Just ann) = case findComment ann (EP.G EP.AnnCloseP) of
        Nothing -> ""
        Just c  -> " " ++ printComment c
  printExpr _ = ""

printType (TyVar  name sit) = printName name
printType (TyApps apt  si ) = printComment' (getAnn si)
                                            (unwords $ printAppType <$> apt)
 where
  printAppType (AppPrefix t    si ) = printComment' (getAnn si) $ printType t
  printAppType (AppInfix  name si2) = printName name
printType (TyApp t1 t2 si) = printType t1 ++ " " ++ printType t2
printType (TyFun t1 t2 si) =
  printComment' ann $ printType t1 ++ printArAnn ann ++ " -> " ++ printType t2
 where
  ann = getAnn si
  printArAnn Nothing  = ""
  printArAnn (Just a) = case findComment a (EP.G EP.AnnRarrow) of
    Nothing -> ""
    Just c  -> " " ++ printComment c
printType (TyList t1 si) =
  "[" ++ printType t1 ++ printCloseAnn (getAnn si) ++ "]"
  where
    printCloseAnn Nothing    = ""
    printCloseAnn (Just ann) = case findComment ann (EP.G EP.AnnCloseS) of
      Nothing -> ""
      Just c  -> " " ++ printComment c
printType (TyTuple ts si) = "(" ++ intercalate ", " (printType <$> ts) ++ printCloseAnn (getAnn si) ++ ")"
  where
    printCloseAnn Nothing    = ""
    printCloseAnn (Just ann) = case findComment ann (EP.G EP.AnnCloseP) of
      Nothing -> ""
      Just c  -> " " ++ printComment c



printSig :: Maybe EP.Annotation -> Sig -> String
printSig Nothing (TypeSig names tp si) =
  show si
    ++ "  !!! "
    ++ intercalate ", " (printName <$> names)
    ++ " :: "
    ++ printType tp
    ++ ";"
printSig (Just sann) (TypeSig names tp si) =
  intercalate ", " (printName <$> names)
    ++ printSigComm
    ++ " :: "
    ++ printType tp
    ++ ";"
 where
  printSigComm = case findComment sann (EP.G EP.AnnDcolon) of
    Nothing -> ""
    Just c  -> " " ++ printComment c
