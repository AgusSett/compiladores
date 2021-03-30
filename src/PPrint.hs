{-|
Module      : PPrint
Description : Pretty printer para PCF.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module PPrint (
    pp,
    ppTy,
    ppName
    ) where

import Lang
import Subst ( openN )
import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, bold, color, colorDull, Color (..), AnsiStyle )
import Data.Text.Prettyprint.Doc
  ( (<+>), nest, parens, sep, pretty, Doc, layoutSmart, defaultLayoutOptions, annotate )

-- Como `openN`, pero cambia el nombre si genera shadowing. Nota:
-- esto es rídiculamente ineficiente si los términos empiezan a ser
-- grandes, porque requiere atravesarlo íntegramente.
openRename :: [Name] -> Term -> ([Name], Term)
openRename ns t =
  let fs = freeVars t in
  let freshen n = let cands = n : (map (\i -> n ++ show i) [0..]) in
                  let n' = head (filter (\m -> not (elem m fs)) cands) in
                  n'
  in
  let fresh_ns = map freshen ns in
  (fresh_ns, openN fresh_ns t)

-- | 'openAll' convierte términos locally nameless
-- a términos fully named abriendo todos las variables de ligadura que va encontrando
-- Debe tener cuidado de no abrir términos con nombres que ya fueron abiertos.
openAll :: Term -> NTerm
openAll (V p v) = case v of 
      Bound i ->  V p $ "(Bound "++show i++")" --este caso no debería aparecer
                                               --si el término es localmente cerrado
      Free x -> V p x 
openAll (Const p c) = Const p c
openAll (Lam p x ty t) =
    let ([x'], t') = openRename [x] t in
    Lam p x' ty (openAll t')
openAll (App p t u) = App p (openAll t) (openAll u)
openAll (Fix p f fty x xty t) =
    let ([f', x'], t') = openRename [f, x] t in
    Fix p f' fty x' xty (openAll t')
openAll (IfZ p c t e) = IfZ p (openAll c) (openAll t) (openAll e)
openAll (BinaryOp i o a b) = BinaryOp i o (openAll a) (openAll b)
openAll (Let p n ty t t') =
    let ([n'], t'') = openRename [n] t' in
    Let p n' ty (openAll t) (openAll t'')

-- | Pretty printer de nombres (Doc)
name2doc :: Name -> Doc AnsiStyle
name2doc n = pretty n

-- |  Pretty printer de nombres (String)
ppName :: Name -> String
ppName = id

-- | Pretty printer para tipos (Doc)
ty2doc :: Ty -> Doc AnsiStyle
ty2doc NatTy     = annotate (color Blue <> italicized) (pretty "Nat")
ty2doc (FunTy x@(FunTy _ _) y) = sep [parens (ty2doc x), annotate (colorDull Blue) (pretty "->"),ty2doc y]
ty2doc (FunTy x y) = sep [ty2doc x, annotate (colorDull Blue) (pretty "->"),ty2doc y] 

-- | Pretty printer para tipos (String)
ppTy :: Ty -> String
ppTy = unpack . renderStrict . layoutSmart defaultLayoutOptions . ty2doc

c2doc :: Const -> Doc AnsiStyle
c2doc (CNat n) = annotate (colorDull Green) (pretty (show n))

binary2doc :: BinaryOp -> Doc AnsiStyle
binary2doc Plus = annotate (color Red <> bold) (pretty "+")
binary2doc Minus = annotate (color Red <> bold) (pretty "-")

collectApp :: NTerm -> (NTerm, [NTerm])
collectApp t = go [] t where
  go ts (App _ h tt) = go (tt:ts) h
  go ts h = (h, ts)

parenIf :: Bool -> Doc AnsiStyle -> Doc AnsiStyle
parenIf True = parens
parenIf _ = id

-- t2doc at t :: Doc
-- at: debe ser un átomo
-- | Pretty printing de términos (Doc)
t2doc :: Bool     -- Debe ser un átomo? 
      -> NTerm    -- término a mostrar
      -> Doc AnsiStyle
-- Uncomment to use the Show instance for STerm
{- t2doc at x = text (show x) -}
t2doc at (V _ x) = pretty x
t2doc at (Const _ c) = c2doc c
t2doc at (Lam _ v ty t) =
  parenIf at $
  sep [sep [annotate (color Red) (pretty "λ"), parens (sep [name2doc v,pretty ":",ty2doc ty]), annotate (color Red) (pretty "->")], nest 2 (t2doc False t)]

t2doc at t@(App _ _ _) =
  let (h, ts) = collectApp t in
  parenIf at $
  t2doc True h <+> sep (map (t2doc True) ts)

t2doc at (Fix _ f fty x xty m) =
  parenIf at $
  sep [ sep [annotate (color Red) (pretty "fix"), binding2doc (f, fty), binding2doc (x, xty), annotate (color Red) (pretty "->") ]
      , nest 2 (t2doc False m)
      ]

t2doc at (IfZ _ c t e) =
  parenIf at $
  sep [annotate (color Red) (pretty "ifz"), nest 2 (t2doc False c)
     , annotate (color Red) (pretty "then"), nest 2 (t2doc False t)
     , annotate (color Red) (pretty "else"), nest 2 (t2doc False e) ]

t2doc at (BinaryOp _ o a b) =
  parenIf at $
  t2doc True a <+> binary2doc o <+> t2doc True b

t2doc at (Let _ v ty t t') =
  parenIf at $
  sep [
    sep [annotate (color Red) (pretty "let")
       , name2doc v, pretty ":", ty2doc ty
       , annotate (color Red) (pretty "=") ]
  , nest 2 (t2doc False t), annotate (color Red) (pretty "in"), nest 2 (t2doc False t') ]

binding2doc :: (Name, Ty) -> Doc AnsiStyle
binding2doc (x, ty) =
  parens (sep [name2doc x, pretty ":", ty2doc ty])

-- | Pretty printing de términos (String)
pp :: Term -> String
-- Uncomment to use the Show instance for Term
{- pp = show -}
pp = unpack . renderStrict . layoutSmart defaultLayoutOptions . t2doc False . openAll


