{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AnnConverter where


import CommonTtgAST
import SrcInfo
import qualified AnnTtgAST as ATA

toAnn :: Hs -> ATA.Hs
toAnn (Hs n i d si) = ATA.Hs (toAnnName <$> n) (toAnnImport <$> i) (toAnnDecl <$> d) (toAnnSrcInfo <$> si)
  where
    toAnnSrcInfo (SrcInfo ann _) = ATA.SrcInfo ann

    toAnnName (Name  str si) = ATA.Name str (toAnnSrcInfo <$> si)

    toAnnImport (Import n q a si) = ATA.Import (toAnnName n) q (toAnnName <$> a) (toAnnSrcInfo <$> si)

    toAnnDecl (ValDecl  b si) = ATA.ValDecl (toAnnBind b) (toAnnSrcInfo <$> si)
    toAnnDecl (SigDecl  s si) = ATA.SigDecl (toAnnSig s) (toAnnSrcInfo <$> si)

    toAnnSig (TypeSig ns t si) = ATA.TypeSig (toAnnName <$> ns) (toAnnType t) (toAnnSrcInfo <$> si)

    toAnnType (TyVar n si)     = ATA.TyVar (toAnnName n) (toAnnSrcInfo <$> si)
    toAnnType (TyApps ats si)  = ATA.TyApps (toAnnAppType <$> ats) (toAnnSrcInfo <$> si)
    toAnnType (TyApp t1 t2 si) = ATA.TyApp (toAnnType t1) (toAnnType t2) (toAnnSrcInfo <$> si)
    toAnnType (TyFun t1 t2 si) = ATA.TyFun (toAnnType t1) (toAnnType t2) (toAnnSrcInfo <$> si)
    toAnnType (TyTuple ts si)  = ATA.TyTuple (toAnnType <$> ts) (toAnnSrcInfo <$> si)
    toAnnType (TyList t si)    = ATA.TyList (toAnnType t) (toAnnSrcInfo <$> si)

    toAnnAppType (AppInfix n si)  = ATA.AppInfix (toAnnName n) (toAnnSrcInfo <$> si)
    toAnnAppType (AppPrefix t si) = ATA.AppPrefix (toAnnType t) (toAnnSrcInfo <$> si)

    toAnnBind (FunBind n ms si) = ATA.FunBind (toAnnName n) (toAnnMatch <$> ms) (toAnnSrcInfo <$> si)

    toAnnMatch (Match args grhss locals si) = ATA.Match (toAnnPat <$> args)
                                                       (toAnnGrhs <$> grhss)
                                                       (toAnnLocal locals)
                                                       (toAnnSrcInfo <$> si)

    toAnnGrhs (GRHS s e msi) = ATA.GRHS (toAnnStmt <$> s) (toAnnExpr e) (toAnnSrcInfo <$> msi)

    toAnnPat (VarPat n si)       = ATA.VarPat (toAnnName n) (toAnnSrcInfo <$> si)
    toAnnPat (ParPat p si)       = ATA.ParPat (toAnnPat p) (toAnnSrcInfo <$> si)
    toAnnPat (NPat ol si)        = ATA.NPat (toAnnOverLit ol) (toAnnSrcInfo <$> si)
    toAnnPat (ConstPat n cpd si) = ATA.ConstPat (toAnnName n) (toAnnConstPatDet cpd) (toAnnSrcInfo <$> si)
    toAnnPat (WildPat si)        = ATA.WildPat (toAnnSrcInfo <$> si)

    toAnnConstPatDet (PrefixConPat ps)   = ATA.PrefixConPat (toAnnPat <$> ps) 
    toAnnConstPatDet (InfixConPat p1 p2) = ATA.InfixConPat (toAnnPat p1) (toAnnPat p2) 

    toAnnStmt (BindStmt p1 expr si) = ATA.BindStmt (toAnnPat p1) (toAnnExpr expr) (toAnnSrcInfo <$> si)
    toAnnStmt (BodyStmt expr si)    = ATA.BodyStmt (toAnnExpr expr) (toAnnSrcInfo <$> si)
    toAnnStmt (LetStmt lb si)       = ATA.LetStmt (toAnnLocal lb) (toAnnSrcInfo <$> si)

    toAnnLocal (ValLocalBind binds sigs) = ATA.ValLocalBind (toAnnBind <$> binds) (toAnnSig <$> sigs) 
    toAnnLocal EmptyLocalBind            = ATA.EmptyLocalBind

    toAnnExpr (Var n si)                   = ATA.Var (toAnnName n) (toAnnSrcInfo <$> si)
    toAnnExpr (OverLit ol si)              = ATA.OverLit (toAnnOverLit ol) (toAnnSrcInfo <$> si)
    toAnnExpr (Lit lit si)                 = ATA.Lit (toAnnLit lit) (toAnnSrcInfo <$> si)
    toAnnExpr (Lam match si)               = ATA.Lam (toAnnMatch match) (toAnnSrcInfo <$> si)
    toAnnExpr (App expr1 expr2 si)         = ATA.App (toAnnExpr expr1) (toAnnExpr expr2) (toAnnSrcInfo <$> si)
    toAnnExpr (OpApp expr1 expr2 expr3 si) = ATA.OpApp (toAnnExpr expr1) (toAnnExpr expr2) (toAnnExpr expr3) (toAnnSrcInfo <$> si)
    toAnnExpr (Let lb expr si)             = ATA.Let (toAnnLocal lb) (toAnnExpr expr) (toAnnSrcInfo <$> si)
    toAnnExpr (If expr1 expr2 expr3 si)    = ATA.If (toAnnExpr expr1) (toAnnExpr expr2) (toAnnExpr expr3) (toAnnSrcInfo <$> si)
    toAnnExpr (Do stmts si)                = ATA.Do (toAnnStmt <$> stmts) (toAnnSrcInfo <$> si)
    toAnnExpr (Case expr match si)         = ATA.Case (toAnnExpr expr) (toAnnMatch match) (toAnnSrcInfo <$> si)
    toAnnExpr (ExprWithType expr typ si)   = ATA.ExprWithType (toAnnExpr expr) (toAnnType typ) (toAnnSrcInfo <$> si)
    toAnnExpr (Par expr si)                = ATA.Par (toAnnExpr expr) (toAnnSrcInfo <$> si)

    toAnnLit (LitChar ch)    = ATA.LitChar ch
    toAnnLit (LitString str) = ATA.LitString str
    
    toAnnOverLit (OverLitInteger i)    = ATA.OverLitInteger i 
    toAnnOverLit (OverLitFractional r) = ATA.OverLitFractional r 
    toAnnOverLit (OverLitString s)     = ATA.OverLitString s 
