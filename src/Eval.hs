{-|
Module      : Eval
Description : Evalúa un término siguiendo la semántica big-step
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo evaluá términos siguiendo la semántica big-step (estrategia CBV)
-}

module Eval where

import Common ( abort )
import Lang
import Subst ( substN, subst )
import MonadPCF ( MonadPCF, lookupDecl, failPCF )
import PPrint ( ppName )

-- | Evaluador de términos CBV
eval ::  MonadPCF m => Term -> m Term
eval (V _ (Free nm)) = do
  -- unfold and keep going
  mtm <- lookupDecl nm 
  case mtm of 
    Nothing -> failPCF $ "Error de ejecución: variable no declarada: " ++ ppName nm 
    Just t -> eval t

eval (App p l r) = do
     le <- eval l
     re <- eval r
     case (le, re) of
        (Lam _ y _ m, n) ->
           eval (subst n m)
        (ff@(Fix _ f _ _ _ t), n) ->
           eval (substN [ff, n] t)
        _ ->
           abort("Error de tipo en runtime " ++ show (le, re))

eval (BinaryOp p o a b) = do
        ae <- eval a
        be <- eval b
        case (ae, be) of
          (Const _ (CNat n), Const _ (CNat m)) -> return (Const p (CNat (mapOp o n m)))
          (_, _)               -> abort ("Error de tipo en runtime!")

eval (IfZ p c t e) = do
     ce <- eval c
     case ce of
       Const _ (CNat 0) -> eval t
       Const _ (CNat _) -> eval e
       c' -> abort ("Error de tipo en runtime!")

-- nada más para reducir
eval t = return t

mapOp :: BinaryOp -> (Int -> Int -> Int)
mapOp Plus = (+)
mapOp Minus = (-)