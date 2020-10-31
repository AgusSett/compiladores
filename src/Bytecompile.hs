{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Byecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM 
para ejecutar bytecode.
-}
module Bytecompile
  (Bytecode, bytecompileModule, runBC, bcWrite, bcRead)
 where

import Lang 
import Subst
import MonadPCF
import Elab ( elab )

import qualified Data.ByteString.Lazy as BS
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )

type Opcode = Int
type Bytecode = [Int]

newtype Bytecode32 = BC { un32 :: [Word32] }

type Module = [Decl NTerm]
type Env = [Val]
data Val = I Int 
         | Fun Env Bytecode
         | RA Env Bytecode
           deriving ( Show )

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go 
    where go =  
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:
 
   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern PLUS     = 6
pattern MINUS    = 7
pattern IFZ      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern JUMP     = 11
pattern SHIFT    = 12
pattern DROP     = 13
pattern PRINT    = 14
pattern TAILCALL = 15

mapOp :: BinaryOp -> Int
mapOp Plus = PLUS
mapOp Minus = MINUS

bc :: MonadPCF m => Term -> m Bytecode
bc (V _ (Bound x))    = return [ACCESS, x]
bc (V _ (Free nm))    = do
  -- unfold and keep going
  mtm <- lookupDecl nm 
  case mtm of 
    Nothing -> failPCF $ "Error de ejecución: variable no declarada: " ++ nm 
    Just t -> bc t
bc (Const _ (CNat n)) = return [CONST, n]
bc (Lam _ _ _ t)      = do cf <- tc t
                           return ([FUNCTION, (length cf)] ++ cf)
bc (Fix _ _ _ _ _ t)  = do cf <- bc t
                           return ([FUNCTION, (length cf) + 1] ++ cf ++ [RETURN, FIX])
bc (BinaryOp _ o a b) = do ca <- bc a
                           cb <- bc b
                           return (ca ++ cb ++ [mapOp o])
bc (IfZ _ c t e)      = do cc <- bc c
                           ce <- bc e
                           ct <- bc t
                           return (cc ++ [IFZ, (length ct) + 2] ++ ct ++ [JUMP, length ce] ++ ce)
bc (App _ t u)        = do ct <- bc t
                           cu <- bc u
                           return (ct ++ cu ++ [CALL])
bc (Let _ _ _ e1 e2)  = do ce1 <- bc e1
                           ce2 <- bc e2
                           return (ce1 ++ [SHIFT] ++ ce2 ++ [DROP])

tc :: MonadPCF m => Term -> m Bytecode
tc (App _ t u) = do ct <- bc t
                    cu <- bc u
                    return (ct ++ cu ++ [TAILCALL])
tc (Let _ _ _ e1 e2)  = do ce1 <- bc e1
                           ce2 <- tc e2
                           return (ce1 ++ [SHIFT, ACCESS, 0] ++ ce2)
tc (IfZ _ c t e) = do cc <- bc c
                      ct <- tc t
                      ce <- tc e
                      return (cc ++ [IFZ, (length ct)] ++ ct ++ ce) 
tc t           = (++) <$> bc t <*> return [RETURN]

bytecompileModule :: MonadPCF m => Module -> m Bytecode
bytecompileModule mod = do let tp = elab (foldLet mod) -- TODO: elab aca?
                           btc <- bc tp
                           return (btc ++ [PRINT, STOP])

foldLet :: [Decl NTerm] -> NTerm
foldLet [d]    = (declBody d)
foldLet (d:xs) = Let (declPos d) (declName d) (declTy d) (declBody d) (foldLet xs)

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename

runBC :: MonadPCF m => Bytecode -> m ()
runBC c = run c [] []

run :: MonadPCF m => Bytecode -> Env -> Env -> m ()
run (STOP:_) _ _                 = return ()
run (CONST:n:c) e s              = run c e ((I n):s)
run (MINUS:c) e ((I n):(I m):s)  = run c e ((I (max 0 (n-m))):s)
run (PLUS:c) e ((I n):(I m):s)   = run c e ((I (n+m)):s)
run (ACCESS:i:c) e s             = run c e ((e!!i):s)
run (CALL:c) e (v:(Fun ef cf):s) = run cf (v:ef) ((RA e c):s)
run (TAILCALL:_) _ (v:(Fun ef cf):s) = run cf (v:ef) s
run (FUNCTION:n:c) e s           = run (drop n c) e ((Fun e (take n c)):s)
run (RETURN:_) _ (v:(RA e c):s)  = run c e (v:s)
run (PRINT:c) e ((I n):s)        = printPCF (show n) >> run c e ((I n):s)
run (FIX:c) e ((Fun ef cf):s)    = let fix = (Fun fix cf):ef in run c e ((Fun fix cf):s)
run (SHIFT:c) e (v:s)            = run c (v:e) s
run (DROP:c) (v:e) s             = run c e s
run (JUMP:n:c) e s               = run (drop n c) e s
run (IFZ:n:c) e ((I 0):s)        = run c e s
run (IFZ:n:c) e ((I _):s)        = run (drop n c) e s