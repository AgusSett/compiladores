module Closure where

import Lang
import Subst ( open, openN )
import Data.List
import Control.Monad.State
import Control.Monad.Writer

data IrDecl = IrVal Name Ir
            | IrFun Name [Name] Ir
            deriving ( Show )

data Ir = IrVar Name
        | IrCall Ir [Ir]
        | IrConst Const
        | IrBinaryOp BinaryOp Ir Ir
        | IrLet Name Ir Ir
        | IrIfZ Ir Ir Ir
        | MkClosure Name [Ir]
        | IrAccess Ir Int
        deriving ( Show )

uniq :: Eq a => [a] -> [a]
uniq (x:xs) = x : uniq (filter (/= x) xs)
uniq [] = []

freeVars' :: Term -> [Name]
freeVars' t = uniq $ filter (isPrefixOf "__") (freeVars t)

foldLet :: [Name] -> Name -> Ir -> Ir
foldLet vars clo t = foldr (\(n, i) x -> IrLet n (IrAccess (IrVar clo) i) x) t (zip vars [1..])

closureConvert :: Term -> StateT Int (Writer [IrDecl]) Ir
closureConvert (V _ (Free n))       = return (IrVar n)
closureConvert (Const _ c)          = return (IrConst c)
closureConvert (Lam _ v _ t)        =
    do  n <- fresh ""
        v' <- fresh v
        clo <- fresh "clo"
        let vars = freeVars' t
        t' <- closureConvert (open v' t)
        let free = foldLet vars clo t'
        tell [IrFun n [clo, v'] free]
        return (MkClosure n (map IrVar vars))
closureConvert (Fix _ f _ x _ t) = 
    do  n <- fresh ""
        x' <- fresh x
        f' <- fresh f
        clo <- fresh "clo"
        let vars = freeVars' t
        t' <- closureConvert (openN [f', x'] t)
        let free = foldLet vars clo t'
        tell [IrFun n [clo, x'] (IrLet f' (IrVar clo) free)]
        return (MkClosure n (map IrVar vars))
closureConvert (App _ (V _ (Free n)) a) =
    do a' <- closureConvert a
       return (IrCall (IrAccess (IrVar n) 0) [IrVar n, a'])
closureConvert (App _ h a)           =
    do h' <- closureConvert h
       a' <- closureConvert a
       clo <- fresh "clo"
       return (IrLet clo h' (IrCall (IrAccess (IrVar clo) 0) [IrVar clo, a']))
        
closureConvert (IfZ _ c t e)         = IrIfZ <$> closureConvert c <*> closureConvert t <*> closureConvert e
closureConvert (BinaryOp _ o a b)    = IrBinaryOp o <$> closureConvert a <*> closureConvert b
closureConvert (Let _ v _ t t')      =
    do  v' <- fresh v
        t2 <- closureConvert t
        t2' <- closureConvert (open v' t')
        return $ IrLet v' t2 t2'

fresh :: Monad m => String -> StateT Int m Name
fresh n = 
    do g <- get
       modify (+1)
       return ("__" ++ n ++ show g)

convertDecl :: Decl Term -> StateT Int (Writer [IrDecl]) ()
convertDecl (Decl _ n _ t) = closureConvert t >>= (\t' -> tell [IrVal n t'])

runCC :: [Decl Term] -> [IrDecl]
runCC xs = execWriter $ evalStateT (mapM convertDecl xs) 0