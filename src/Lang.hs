{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definiciones de distintos tipos de datos:
  - AST de términos
  - Declaraciones
  - Tipos
  - Variables

-}

module Lang where

import Common ( Pos )

-- | AST de Tipos
data Ty = 
      NatTy 
    | FunTy Ty Ty
    deriving (Show,Eq)

type Name = String

data Const = CNat Int
  deriving Show

data UnaryOp = Succ | Pred
  deriving Show

data BinaryOp = Plus | Minus
  deriving Show

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl a =
    Decl { declPos :: Pos, declName :: Name, declTy :: Ty, declBody :: a }
  deriving (Show,Functor)

-- | AST de los términos. 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 
data Tm info var = 
    V info var
  | Const info Const
  | Lam info Name Ty (Tm info var)
  | App info (Tm info var) (Tm info var)
  | BinaryOp info BinaryOp (Tm info var) (Tm info var)
  | Fix info Name Ty Name Ty (Tm info var)
  | IfZ info (Tm info var) (Tm info var) (Tm info var)
  | Let info Name Ty (Tm info var) (Tm info var)
  deriving (Show, Functor)

type NTerm = Tm Pos Name   -- ^ 'Tm' tiene 'Name's como variables ligadas y libres, guarda posición
type Term = Tm Pos Var     -- ^ 'Tm' con índices de De Bruijn como variables ligadas, different type of variables, guarda posición

data Var = 
    Bound !Int
  | Free Name
  deriving Show

-- | Obtiene la info en la raíz del término.
getInfo :: Tm info var -> info
getInfo (V i _) = i
getInfo (Const i _) = i
getInfo (Lam i _ _ _) = i
getInfo (App i _ _ ) = i
getInfo (BinaryOp i _ _ _) = i
getInfo (Fix i _ _ _ _ _) = i
getInfo (IfZ i _ _ _) = i

-- | Obtiene las variables libres de un término.
freeVars :: Tm info Var -> [Name]
freeVars (V _ (Free v))    = [v]
freeVars (V _ _)           = []
freeVars (Lam _ _ _ t)     = freeVars t
freeVars (App _ l r)       = freeVars l ++ freeVars r
freeVars (BinaryOp _ _ a b) = freeVars a ++ freeVars b
freeVars (Fix _ _ _ _ _ t) = freeVars t
freeVars (IfZ _ c t e)     = freeVars c ++ freeVars t ++ freeVars e
freeVars (Const _ _)       = []
freeVars (Let _ _ _ t t')  = freeVars t ++ freeVars t'

data STy = 
      SNatTy 
    | SFunTy STy STy
    | SSynTy Name
    deriving (Show)

-- | AST superficial. 
--   - info es información extra que puede llevar cada nodo. 
data STm info =
    SV info Name
  | SConst info Const
  | SLam info [(Name, STy)] (STm info)
  | SApp info (STm info) (STm info)
  | SUnaryOp info UnaryOp (Maybe (STm info))
  | SBinaryOp info BinaryOp (STm info) (STm info)
  | SFix info Name STy [(Name, STy)] (STm info)
  | SIfZ info (STm info) (STm info) (STm info)
  | SLet info Name STy [(Name, STy)] (STm info) (STm info)
  | SLetRec info Name STy [(Name, STy)] (STm info) (STm info)
  deriving (Show)

type STerm = STm Pos

data SDecl =
    SDeclTm Pos Name STy [(Name, STy)] STerm
  | SDeclRec Pos Name STy [(Name, STy)] STerm
  | SDeclSyn Pos Name STy
  deriving (Show)
