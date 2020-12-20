module CIR where

import Lang ( BinaryOp, Name, UnaryOp, Const ( CNat ) )
import Data.List (intercalate)
import Closure

newtype Reg = Temp String
  deriving Show

data Val = R Reg | C Int | G Name
  deriving Show

type Loc = String

data Inst =
    Assign Reg Expr
  | Store Name Expr
  deriving Show

data Expr =
    BinOp BinaryOp Val Val
  | UnOp UnaryOp Val
  | Phi [(Loc, Val)]
  | Call Val [Val]
  | MkClosure Loc [Val]
  | V Val
  | Access Val Int
  deriving Show

data Terminator =
    Jump Loc
  | CondJump Cond Loc Loc
  | Return Val
  deriving Show

data Cond =
    Eq Val Val
  deriving Show

type BasicBlock = (Loc, [Inst], Terminator)
type Blocks = [BasicBlock]

type CanonFun = (String, [String], [BasicBlock])
type CanonVal = String -- Sólo el nombre, tipo puntero siempre
newtype CanonProg = CanonProg [Either CanonFun CanonVal]

print :: (Blocks, [Inst], Val) -> String
print (bs, is, v) =
  concatMap printBlock bs ++ show is ++ "\n" ++ show v ++ "\n"

printBlock :: BasicBlock -> String
printBlock (loc, is, t) =
  loc ++ ":\n" ++
  concatMap (\i -> "  " ++ show i ++ "\n") is ++
  show t ++ "\n"

instance Show CanonProg where
  show (CanonProg prog) = concatMap pr1 prog where
    pr1 (Left (f, args, blocks)) =
      f ++ "(" ++ intercalate ", " args ++ ") {\n"
        ++ concatMap printBlock blocks ++ "}\n\n"

    pr1 (Right v) =
      "declare " ++ v ++ "\n\n"

{-
runCanon :: [IrDecl] -> CanonProg
runCanon xs = CanonProg (map canonDecl xs)

canonDecl :: IrDecl -> Either CanonFun CanonVal
canonDecl (IrVal x t) = Right x
canonDecl (IrFun f args t) = Left (f, args, canon t)

canonVal :: Ir -> Val
canonVal (IrVar n) | isPrefixOf "__" n = R (Temp n)
canonVal (IrVar n) | otherwise         = G n
canonVal (IrConst (CNat n))            = C n 


canon :: Ir -> [Inst]
canon (IrVar n)          = V (canonVal (IrVar n))
canon (IrConst (CNat n)) = V (canonVal (IrConst (CNat n)))

canon (IrCall f args) = let args' = map canonVal args
                        in Call (canonVal f) 
canon (IrAccess v i) = Access (canonVal v) i
canon (IrBinaryOp o a b) = [
  Assign (Temp "t1") Expr,
  Assign (Temp "t3") (BinOp o (R (Temp "t1")) (R (Temp "t2")))
]
canon IrLet x t t' = Assign 
canon IrIfZ Ir Ir Ir
canon MkClosure Name [Ir]
-}

--pcfmain :: CanonFun
--pcfmain = ("pcfmain", [], [("pcfmain", [Store "var" x, ...], Return (V (C 0)))])