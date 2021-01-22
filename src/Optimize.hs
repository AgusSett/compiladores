module Optimize where

import Lang

constFold :: Term -> Term
constFold t@(BinaryOp i op a b) =
  case (constFold a, constFold b) of
    (Const _ (CNat 0), b') -> b'
    (a', Const _ (CNat 0)) -> a'
    (Const _ (CNat n1), Const _ (CNat n2)) -> Const i (CNat (mapOp op n1 n2))
    _ -> t
constFold t@(IfZ i c a b) =
  case constFold c of
    Const _ (CNat 0) -> a
    Const _ (CNat _) -> b
    _ -> t

mapOp :: BinaryOp -> (Int -> Int -> Int)
mapOp Plus  = (+)
mapOp Minus = (-)

-- inline

-- deadCode
