{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) 
-}

module Elab ( elab, elab_decl, desugar, foldTy, desugarTy ) where

import Lang
import Subst
import MonadPCF
import Common ( Pos(NoPos) )

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: NTerm -> Term
elab (V p v)               = V p (Free v)
elab (Const p c)           = Const p c
elab (Lam p v ty t)        = Lam p v ty (close v (elab t))
elab (App p h a)           = App p (elab h) (elab a)
elab (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab t))
elab (IfZ p c t e)         = IfZ p (elab c) (elab t) (elab e)
elab (UnaryOp i o t)       = UnaryOp i o (elab t)

elab_decl :: Decl NTerm -> Decl Term
elab_decl = fmap elab

desugar :: MonadPCF m => STerm -> m NTerm
desugar (SV p v)                            = return (V p v)
desugar (SConst p c)                        = return (Const p c)
desugar (SLam p [] t)                       = desugar t
desugar (SLam p [(v, ty)] t)                = Lam p v <$> desugarTy ty <*> desugar t
desugar (SLam p ((v, ty):vs) t)             = desugar (SLam p [(v, ty)] (SLam p vs t))
desugar (SApp p h a)                        = App p <$> desugar h <*> desugar a
desugar (SFix p f fty [(x, xty)] t)         = Fix p f <$> desugarTy fty <*> return x <*> desugarTy xty <*> desugar t
desugar (SFix p f fty ((x, ty):xs) t)       = desugar (SFix p f fty [(x, ty)] (SLam p xs t))
desugar (SIfZ p c t e)                      = IfZ p <$> desugar c <*> desugar t <*> desugar e
desugar (SUnaryOp i o (Just t))             = UnaryOp i o <$> desugar t
desugar (SUnaryOp i o Nothing)              = return (Lam i "x" NatTy (UnaryOp i o (V i "x")))
desugar (SLet p v ty [] t t')               = do tty <- desugarTy ty
                                                 tt <- desugar t
                                                 tt' <- desugar t'
                                                 return (App p (Lam p v tty tt') tt)
desugar (SLet p v ty xs t t')               = desugar (SLet p v (foldTy xs ty) [] (SLam p xs t) t')
desugar (SLetRec p v ty [] t t')            = failPosPCF p $ "Se requiere un argumento como minimo: " ++ v
desugar (SLetRec p v ty [(x, xty)] t t')    = desugar (SLet p v (SFunTy xty ty) [] (SFix p v (SFunTy xty ty) [(x, xty)] t) t')
desugar (SLetRec p v ty ((x, xty):xs) t t') = desugar (SLetRec p v (foldTy xs ty) [(x, xty)] (SLam p xs t) t')

foldTy :: [(Name, STy)] -> STy -> STy
foldTy xs xty = foldr (\(_, ty) ty' -> SFunTy ty ty') xty xs

desugarTy :: MonadPCF m => STy -> m Ty
desugarTy SNatTy          = return NatTy
desugarTy (SFunTy ty ty') = FunTy <$> desugarTy ty <*>  desugarTy ty'
desugarTy (SSynTy n)      = do syn <- lookupTySyn n
                               case syn of
                                 Nothing -> failPosPCF NoPos $ "Sinonimo de tipo no declarado: " ++ n
                                 Just ty -> return ty 