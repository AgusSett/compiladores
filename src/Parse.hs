{-|
Module      : Parse
Description : Define un parser de términos PCF0 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language ( GenLanguageDef(..), emptyDef )

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "in", "type", "fun", "fix", "rec", "then", "else", 
                          "succ", "pred", "ifz", "Nat"],
         reservedOpNames = ["->",":","="]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer 
natural = Tok.natural lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier 

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyvar :: P Name
tyvar = do c <- upper
           cs <- many alphaNum
           whiteSpace
           return (c:cs)

tyatom :: P STy
tyatom = (reserved "Nat" >> return SNatTy)
         <|> parens typeP
         <|> (SSynTy <$> tyvar)

typeP :: P STy
typeP = try (do 
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (SFunTy x y))
      <|> tyatom
          
const :: P Const
const = CNat <$> num

unaryOp :: P STerm
unaryOp = do
  i <- getPos
  foldr (\(w, r) rest -> try (do 
                                 reserved w
                                 a <- optionMaybe atom
                                 return (r a)) <|> rest) parserZero (mapping i)
  where
   mapping i = [
       ("succ", SUnaryOp i Succ)
     , ("pred", SUnaryOp i Pred)
    ]

atom :: P STerm
atom =     (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> unaryOp
       <|> parens tm

lam :: P STerm
lam = do i <- getPos
         reserved "fun"
         binds <- bindings
         reservedOp "->"
         t <- tm
         return (SLam i binds t)

-- Nota el parser app también parsea un solo atom.
app :: P STerm
app = (do i <- getPos
          f <- atom
          args <- many atom
          return (foldl (SApp i) f args))

ifz :: P STerm
ifz = do i <- getPos
         reserved "ifz"
         c <- tm
         reserved "then"
         t <- tm
         reserved "else"
         e <- tm
         return (SIfZ i c t e)

math :: P STerm
math = do i <- getPos
          chainl1 tm
            ((reservedOp "+" >> return (SBinaryOp i Plus)) <|>
             (reservedOp "-" >> return (SBinaryOp i Minus)))

binding :: P (Name, STy)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)

multibinding :: P [(Name, STy)]
multibinding = do vs <- many1 var -- Multibinders
                  reservedOp ":"
                  ty <- typeP
                  return (map (\v -> (v, ty)) vs)

bindings :: P [(Name, STy)]
bindings = concat <$> many (parens multibinding)

fix :: P STerm
fix = do i <- getPos
         reserved "fix"
         (f, fty) <- parens binding
         xs <- bindings
         reservedOp "->"
         t <- tm
         return (SFix i f fty xs t)

letTm :: P STerm
letTm = do i <- getPos
           reserved "let"
           b <- optionMaybe (reserved "rec")
           f <- var
           binds <- bindings
           reservedOp ":"
           ty <- typeP
           reservedOp "="
           t <- tm
           reserved "in"
           t' <- tm
           case b of
             Nothing -> return (SLet i f ty binds t t')
             Just _  -> return (SLetRec i f ty binds t t')

-- | Parser de términos
tm :: P STerm -- TODO: precedencia de la suma?
tm = chainl1 (app <|> lam <|> ifz <|> unaryOp <|> fix <|> letTm)
             ((reservedOp "+" >> return (SBinaryOp NoPos Plus)) <|>
              (reservedOp "-" >> return (SBinaryOp NoPos Minus)))

synon :: P SDecl
synon = do i <- getPos
           reserved "type"
           n <- tyvar
           reservedOp "="
           ty <- typeP
           return (SDeclSyn i n ty)

-- | Parser de declaraciones
decl :: P SDecl
decl = do 
     i <- getPos
     reserved "let"
     b <- optionMaybe (reserved "rec")
     v <- var
     binds <- bindings
     reservedOp ":"
     ty <- typeP
     reservedOp "="
     t <- tm
     case b of
       Nothing -> return (SDeclTm i v ty binds t)
       Just _  -> return (SDeclRec i v ty binds t)

-- | Parser de programas (listas de declaraciones) 
program :: P [SDecl]
program = many (synon <|> decl)

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either SDecl STerm)
declOrTm = try (Right <$> tm) <|> (Left <$> decl)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP tm s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)