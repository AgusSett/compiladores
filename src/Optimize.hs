module Optimize where

import Lang
import MonadPCF
import Global
import Subst ( subst, substN )

-- const folding
constFold :: Term -> Term
constFold (BinaryOp i op a b) =
  case (constFold a, constFold b) of
    (a', Const _ (CNat 0)) -> a'
    (Const _ (CNat n1), Const _ (CNat n2)) -> Const i (CNat (mapOp op n1 n2))
    (a', b') -> BinaryOp i op a' b'
constFold t@(IfZ i c a b) =
  case constFold c of
    Const _ (CNat 0) -> a
    Const _ (CNat _) -> b
    _ -> t
constFold (Lam i x ty t) = Lam i x ty (constFold t)
constFold (App i h t) = App i (constFold h) (constFold t)
constFold (Fix i f fty x ty t) = Fix i f fty x ty (constFold t)
constFold (Let i n ty t t') = Let i n ty (constFold t) (constFold t')
constFold t = t

constFoldDecl :: Decl Term -> Decl Term
constFoldDecl (Decl i n ty t) = Decl i n ty (constFold t)

mapOp :: BinaryOp -> (Int -> Int -> Int)
mapOp Plus  = (+)
mapOp Minus = \x y -> max 0 (x - y)

-- inline
inline :: MonadPCF m => Term -> m Term
inline (V i (Free n)) =
  do mtm <- lookupDecl n 
     case mtm of 
       Nothing               -> return (V i (Free n))
       Just t@(Const _ _)    -> return t
       Just t@(V _ (Free _)) -> return t
       Just t@(Lam _ _ _ m)     | depth m < inlineSize -> return t
       Just t@(Fix _ _ _ _ _ m) | depth m < inlineSize -> return t
       Just _  -> return (V i (Free n))
inline t@(V _ _)      = return t
inline t@(Const _ _)  = return t
inline (Lam i x ty t) = Lam i x ty <$> inline t
inline (App i h@(Lam _ _ _ m) t)     | depth t < inlineSize = return (subst t m)
inline (App i h@(Fix _ _ _ _ _ m) t) | depth t < inlineSize = return (substN [h, t] m)
inline (App i h t)    = App i <$> inline h <*> inline t
inline (IfZ i c l r)  = IfZ i <$> inline c <*> inline l <*> inline r
inline (BinaryOp i op a b)  = BinaryOp i op <$> inline a <*> inline b
inline (Fix i f fty x ty t) = Fix i f fty x ty <$> inline t
inline (Let i n ty t t')    = Let i n ty <$> inline t <*> inline t'

inlineDecl :: MonadPCF m => Decl Term -> m (Decl Term)
inlineDecl (Decl i n ty t) = Decl i n ty <$> inline t

inlineSize :: Int
inlineSize = 6

depth :: Term -> Int
depth (Lam i x ty t) = 1 + depth t
depth (App i h t)    = 1 + max (depth h) (depth t)
depth (IfZ i c l r)  = 1 + max (depth c) (max (depth l) (depth r))
depth (BinaryOp i op a b)  = 1 + max (depth a) (depth b)
depth (Fix i f fty x ty t) = 1 + depth t
depth (Let i n ty t t')    = 1 + max (depth t) (depth t')
depth _ = 1

-- deadCode
deadCode :: [Decl Term] -> [Name] -> [Decl Term]
deadCode [] _ = []
deadCode (d@(Decl _ nm _ Lam {}) : xs) refs =
  if nm `elem` refs
    then d : deadCode xs refs
    else deadCode xs refs
deadCode (d@(Decl _ nm _ Fix {}) : xs) refs =
  if nm `elem` refs
    then d : deadCode xs refs
    else deadCode xs refs
deadCode (x : xs) refs = x : deadCode xs refs

maxIteration :: Int
maxIteration = 5

optimize :: MonadPCF m => [Decl Term] -> m [Decl Term]
optimize xs =
  do xs'' <- loop xs 0
     let refs = concatMap (freeVars . declBody) xs''
     return $ deadCode xs'' refs
  where loop a n = do xs' <- mapM inlineDecl a
                      let xs'' = map constFoldDecl xs'
                      modify (\s -> s { glb = [] })
                      mapM_ addDecl xs''
                      if n < maxIteration then loop xs'' (n+1) else return xs''
       