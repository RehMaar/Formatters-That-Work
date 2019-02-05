{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleConverter where


import TtgAST
import qualified SimpleTtgAST as STA

toSimpl :: HsX idx -> STA.Hs
toSimpl (HsX idx n i d) = STA.Hs (toSimplName <$> n) (toSimplImport <$> i) (toSimplDecl <$> d)
  where
    toSimplName (NameX _ str) = STA.Name str

    toSimplImport (ImportX _ n q a) = STA.Import (toSimplName n) q (toSimplName <$> a)

    toSimplDecl (ValDeclX _ b) = STA.ValDecl $ toSimplBind b
    toSimplDecl (SigDeclX _ s) = STA.SigDecl $ toSimplSig s

    toSimplSig (TypeSigX _ ns t) = STA.TypeSig (toSimplName <$> ns) (toSimplType t)

    toSimplType (TyVarX _ n)     = STA.TyVar $ toSimplName n
    toSimplType (TyAppsX _ ats)  = STA.TyApps $ toSimplAppType <$> ats
    toSimplType (TyAppX _ t1 t2) = STA.TyApp (toSimplType t1) (toSimplType t2)
    toSimplType (TyFunX _ t1 t2) = STA.TyFun (toSimplType t1) (toSimplType t2)
    toSimplType (TyTupleX _ ts)  = STA.TyTuple (toSimplType <$> ts)
    toSimplType (TyListX _ t)    = STA.TyList (toSimplType t)

    toSimplAppType (AppInfixX _ n)  = STA.AppInfix (toSimplName n)
    toSimplAppType (AppPrefixX _ t) = STA.AppPrefix (toSimplType t)

    toSimplBind (FunBindX _ n ms) = STA.FunBind (toSimplName n) (toSimplMatch <$> ms)

    toSimplMatch (MatchX _ args grhss locals) = STA.Match (toSimplPat <$> args)
                                                          (toSimplGRHS <$> grhss)
                                                          (toSimplLocal locals)
    toSimplGRHS (GRHSX _ s e) = STA.GRHS (toSimplStmt <$> s) (toSimplExpr e)

    toSimplPat (VarPatX _ n)       = STA.VarPat (toSimplName n)
    toSimplPat (ParPatX _ p)       = STA.ParPat (toSimplPat p)
    toSimplPat (NPatX _ ol)        = STA.NPat (toSimplOverLit ol)
    toSimplPat (ConstPatX _ n cpd) = STA.ConstPat (toSimplName n) (toSimplConstPatDet cpd)
    toSimplPat (WildPatX _)        = STA.WildPat

    toSimplConstPatDet (PrefixConPatX _ ps)   = STA.PrefixConPat (toSimplPat <$> ps)
    toSimplConstPatDet (InfixConPatX _ p1 p2) = STA.InfixConPat (toSimplPat p1) (toSimplPat p2)

    toSimplStmt (BindStmtX _ p1 expr) = STA.BindStmt (toSimplPat p1) (toSimplExpr expr)
    toSimplStmt (BodyStmtX _ expr)    = STA.BodyStmt (toSimplExpr expr)
    toSimplStmt (LetStmtX _ lb)       = STA.LetStmt (toSimplLocal lb)

    toSimplLocal (ValLocalBindX _ binds sigs) = STA.ValLocalBind (toSimplBind <$> binds) (toSimplSig <$> sigs)
    toSimplLocal (EmptyLocalBindX _)          = STA.EmptyLocalBind

    toSimplExpr (OverLitX _ ol)              = STA.OverLit (toSimplOverLit ol)
    toSimplExpr (LitX _ lit)                 = STA.Lit (toSimplLit lit)
    toSimplExpr (LamX _ match)               = STA.Lam (toSimplMatch match)
    toSimplExpr (AppX _ expr1 expr2)         = STA.App (toSimplExpr expr1) (toSimplExpr expr2)
    toSimplExpr (OpAppX _ expr1 expr2 expr3) = STA.OpApp (toSimplExpr expr1) (toSimplExpr expr2) (toSimplExpr expr3)
    toSimplExpr (LetX _ lb expr)             = STA.Let (toSimplLocal lb) (toSimplExpr expr)
    toSimplExpr (IfX _ expr1 expr2 expr3)    = STA.If (toSimplExpr expr1) (toSimplExpr expr2) (toSimplExpr expr3)
    toSimplExpr (DoX _ stmts)                = STA.Do (toSimplStmt <$> stmts)
    toSimplExpr (CaseX _ expr match)         = STA.Case (toSimplExpr expr) (toSimplMatch match)
    toSimplExpr (ExprWithTypeX _ expr typ)   = STA.ExprWithType (toSimplExpr expr) (toSimplType typ)

    toSimplLit (LitCharX _ ch)    = STA.LitChar ch
    toSimplLit (LitStringX _ str) = STA.LitString str

    toSimplOverLit (OverLitIntegerX _ i)    = STA.OverLitInteger i
    toSimplOverLit (OverLitFractionalX _ r) = STA.OverLitFractional r
    toSimplOverLit (OverLitStringX _ s)     = STA.OverLitString s
