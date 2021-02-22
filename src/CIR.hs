module CIR where

import Lang ( BinaryOp(Plus),  Name, UnaryOp, Const ( CNat ) )
import Data.List ( intercalate, isPrefixOf )
import Closure hiding ( fresh )
import Control.Monad.State
import Control.Monad.Writer

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
  | Print Val
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


runCanon :: [IrDecl] -> CanonProg
runCanon xs = CanonProg $ map canonDecl xs ++ [Left (pcfmain xs)]

fr :: Monad m => String -> StateT (Int, Loc, [Inst]) m Name
fr n = do (g, l, xs) <- get
          put (g+1, l, xs)
          return (n ++ show g)

inst :: Monad m => Inst -> StateT (Int, Loc, [Inst]) m ()
inst x = modify (\(n, l, xs) -> (n, l, xs ++ [x]))

pushBlock :: Monad m => Loc -> StateT (Int, Loc, [Inst]) m ()
pushBlock l = modify (\(n, _, xs) -> (n, l, xs))

popBlock :: Terminator -> StateT (Int, Loc, [Inst]) (Writer Blocks) ()
popBlock t = do (n, l, xs) <- get
                tell [(l, xs, t)]
                put (n, "", [])

fresh :: Monad m => String -> StateT (Int, Loc, [Inst]) m Name
fresh n = 
    do (g, l, xs) <- get
       put (g+1, l, xs)
       return (n ++ show g)

canonDecl :: IrDecl -> Either CanonFun CanonVal
canonDecl (IrVal x t) = Right x
canonDecl (IrFun f args t) = Left (f, args, execWriter $ evalStateT (canon t >>= exprToVal >>= (popBlock . Return)) (0, "entry", []))

exprToVal :: Expr -> StateT (Int, Loc, [Inst]) (Writer Blocks) Val
exprToVal (V (R r)) = return (R r)
exprToVal (V (G n)) = return (G n)
exprToVal (V (C n)) = return (C n)
exprToVal e = 
  do r <- Temp <$> fresh "r"
     inst $ Assign r e
     return (R r)

canon :: Ir -> StateT (Int, Loc, [Inst]) (Writer Blocks) Expr
canon (IrVar n) | isPrefixOf "__" n = return $ V (R (Temp n))
canon (IrVar n)                     = return $ V (G n)
canon (IrConst (CNat n))            = return $ V (C n)
canon (IrBinaryOp o a b) =
  do a' <- canon a >>= exprToVal
     b' <- canon b >>= exprToVal
     return (BinOp o a' b')

canon (IrAccess v i) =
  do v' <- canon v >>= exprToVal
     return (Access v' i)

canon (IrLet x t t') =
  do a <- canon t
     inst $ Assign (Temp x) a
     canon t'

canon (Closure.MkClosure n args) =
  do args' <- mapM (canon >=> exprToVal) args
     return (CIR.MkClosure n args')

canon (IrCall f args) =
  do f' <- canon f >>= exprToVal
     args' <- mapM (canon >=> exprToVal) args
     return (Call f' args')

canon (IrIfZ c a b) =
  do c' <- canon c >>= exprToVal
     left <- fresh "then"
     right <- fresh "else"
     cont <- fresh "cont"
     popBlock (CondJump (Eq c' (C 0)) left right)

     pushBlock left
     a' <- canon a
     ra <- Temp <$> fresh "r"
     inst $ Assign ra a'

     (_, fromLeft, _) <- get
     popBlock (Jump cont)

     pushBlock right
     b' <- canon b
     rb <- Temp <$> fresh "r"
     inst $ Assign rb b'

     (_, fromRight, _) <- get
     popBlock (Jump cont)

     pushBlock cont
     return (Phi [(fromLeft, R ra), (fromRight, R rb)])


pcfmain :: [IrDecl] -> CanonFun
pcfmain decls = ("pcfmain", [], execWriter $ evalStateT (storeVals vs >>= (popBlock . Return)) (0, "entry", []))
  where vals (IrVal _ _) = True
        vals _ = False
        vs = filter vals decls

storeVals :: [IrDecl] -> StateT (Int, Loc, [Inst]) (Writer Blocks) Val
storeVals [IrVal x t] =
  do v <- canon t >>= exprToVal
     inst (Store x (Print v))
     return (C 0)
storeVals (IrVal x t : xs) = 
  do t' <- canon t
     inst (Store x t')
     storeVals xs