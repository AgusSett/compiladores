{-|
Module      : CEK
Description : Implementacion de la maquina abstracta CEK
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : agustinsettimo.ips@gmail.com
Stability   : experimental

-}

module CEK where

import Lang
import MonadPCF
import PPrint ( ppName )
import Common ( Pos(NoPos) )
import Subst ( substN )

data Val =
    VConst Const
  | VClos Clos
  deriving ( Show )

data Clos =
    ClosFun Env Name Ty Term
  | ClosFix Env Name Ty Name Ty Term
  deriving ( Show )

data Fr =
    KArg Env Term 
  | KIf Env Term Term 
  | KClos Clos
  | KSucc 
  | KPred
  deriving ( Show )

type Env = [Val]
type Kont = [Fr]

search :: MonadPCF m => Term -> Env -> Kont -> m Val
search (UnaryOp _ Pred t) p k = search t p (KPred:k)
search (UnaryOp _ Succ t) p k = search t p (KSucc:k)
search (IfZ _ c t e) p k = search c p ((KIf p t e):k)
search (App _ t u) p k = search t p ((KArg p u):k)

search (V _ (Bound x)) p k = destroy (p!!x) k
search (V _ (Free nm)) p k = do
  -- unfold and keep going
  mtm <- lookupDecl nm 
  case mtm of 
    Nothing -> failPCF $ "Error de ejecución: variable no declarada: " ++ ppName nm 
    Just t -> search t p k
search (Const _ (CNat n)) p k = destroy (VConst (CNat n)) k
search (Lam _ x ty t) p k = destroy (VClos (ClosFun p x ty t)) k
search (Fix _ f fty x ty t) p k = destroy (VClos (ClosFix p f fty x ty t)) k

destroy :: MonadPCF m => Val -> Kont -> m Val
destroy x [] = return x
destroy (VConst (CNat 0)) (KPred:k) = destroy (VConst (CNat 0)) k
destroy (VConst (CNat n)) (KPred:k) = destroy (VConst (CNat (n-1))) k
destroy (VConst (CNat n)) (KSucc:k) = destroy (VConst (CNat (n+1))) k

destroy (VConst (CNat 0)) ((KIf p t e):k) = search t p k
destroy (VConst (CNat _)) ((KIf p t e):k) = search e p k
destroy (VClos clos) ((KArg p t):k) = search t p ((KClos clos):k)
destroy v ((KClos (ClosFun p _ _ t)):k) = search t (v:p) k
destroy v ((KClos (ClosFix p f fty x ty t)):k) = search t ((VClos (ClosFix p f fty x ty t)):v:p) k
destroy v k = failPCF $ "Error de ejecución en destroy: " ++ show v ++ " " ++ show k

valToTerm :: Val -> Term
valToTerm (VConst (CNat n)) = Const NoPos (CNat n)
valToTerm (VClos (ClosFun p x ty t)) = substN (map valToTerm p) (Lam NoPos x ty t)
valToTerm (VClos (ClosFix p f fty x ty t)) = substN (map valToTerm p) (Fix NoPos f fty x ty t)

evalCEK :: MonadPCF m => Term -> m Term
evalCEK t = valToTerm <$> search t [] []