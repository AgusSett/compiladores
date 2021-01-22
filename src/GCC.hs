module GCC ( ppC ) where
import Data.Text.Prettyprint.Doc
import Data.Text ( unpack )
import Data.Text.Prettyprint.Doc.Render.Terminal ( renderStrict )
import Closure
import Lang (BinaryOp(Minus, Plus),  Const(CNat))

decl2doc :: IrDecl -> Doc a
decl2doc (IrVal n t) = pretty "void**" <+> pretty n <> semi
decl2doc (IrFun n args t) =
  pretty "void*" <+> pretty n <+> tupled (map (\x -> pretty "void**" <+> pretty x) args) <+>
  braces (nest 2 (line <> pretty "return" <+> ir2doc t <> semi) <> line)

pcfMain :: [IrDecl] -> Doc a
pcfMain xs = pretty "uint64_t* pcfmain()" <+> braces (nest 2 (line <> vsep (pcfMain' xs ++ [pretty "return 0;"])) <> line)

pcfMain' :: [IrDecl] -> [Doc a]
pcfMain' [] = []
pcfMain' (IrVal n t : xs) = (pretty n <+> pretty "=" <+> ir2doc t <> semi) : pcfMain' xs
pcfMain' (_ : xs) = pcfMain' xs

stmt :: Doc a -> Doc a
stmt x = parens (braces (nest 2 (line <> x <> semi) <> line))

ir2doc :: Ir -> Doc a
ir2doc (IrVar n) = pretty n
ir2doc (IrCall f args) = parens (pretty "(void* (*) (void*, void*))" <+> ir2doc f) <> tupled (map ir2doc args)
ir2doc (IrConst (CNat n)) = pretty n
ir2doc (IrBinaryOp op a b) = pretty "(int)" <+> ir2doc a <+> op2doc op <+> pretty "(int)" <+> ir2doc b
ir2doc (IrLet n t t') = stmt $ hsep [pretty "void**", pretty n, pretty "=",  ir2doc t] <> semi <> line <> ir2doc t'
ir2doc (IrIfZ c a b) = sep [ir2doc c, nest 2 (pretty "?" <+> ir2doc a), nest 2 (colon <+> ir2doc b)]
ir2doc (MkClosure f args) = pretty "pcf_mkclosure" <> tupled (pretty f : pretty (length args) : map ir2doc args)
ir2doc (IrAccess t i) = parens (ir2doc t) <> brackets (pretty i)

op2doc :: BinaryOp -> Doc a
op2doc Plus  = pretty "+"
op2doc Minus = pretty "-"

prelude :: Doc a
prelude = pretty "extern void *pcf_mkclosure(void*, int, ...);"

ppC :: [IrDecl] -> String 
ppC xs = unpack . renderStrict . layoutSmart defaultLayoutOptions $ vsep (prelude : map decl2doc xs ++ [pcfMain xs])
