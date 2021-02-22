{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de PCF.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

--import Control.Monad
import Control.Monad.Trans
import Data.List (nub,  intersperse, isPrefixOf )
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.Environment ( getArgs )
import System.IO ( stderr, hPutStr )
import Options.Applicative

import Global ( GlEnv(..) )
import Errors
import Lang
import Parse ( P, tm, program, declOrTm, runP )
import Elab ( elab, elab_decl, desugar, foldTy, desugarTy )
import Eval ( eval )
import PPrint ( pp , ppTy )
import MonadPCF
import TypeChecker ( tc, tcDecl )
import CEK ( evalCEK )
import Bytecompile ( Bytecode, bcRead, bcWrite, runBC, bytecompileModule )
import Closure ( runCC )
import CIR ( runCanon )
import InstSel ( codegen )
import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy.IO as TIO
import GCC
import Optimize ( optimize )
import GCC ( ppC )
import Data.Text.Lazy (pack)

prompt :: String
prompt = "PCF> "

main :: IO ()
main = execParser opts >>= go
    where
        opts = info (parseArgs <**> helper) 
            ( fullDesc <> progDesc "Compilador de PCF" <> header "Compilador de PCF de la materia Compiladores 2020" )
        go :: (Mode, [FilePath]) -> IO ()
        go (Interactive, files) =
            do runPCF (runInputT defaultSettings (main' files))
               return ()
        go (Typecheck, files) =
            do runPCF $ catchErrors (mapM_ typecheck files)
               return ()
        go (Bytecompile, files) =
            do runPCF $ catchErrors (mapM_ bytecompile files)
               return ()
        go (Run, files) =
            do bc <- mapM bcRead files
               runPCF $ catchErrors (mapM runBC bc)
               return ()
        go (ClosConvert, files) =
            do runPCF $ catchErrors (mapM_ closConvert files)
               return ()
        go (GCC, files) =
            do runPCF $ catchErrors (mapM_ gcc files)
               return ()
        go (CompileLLVM b, files) =
            do runPCF $ catchErrors (mapM_ (compile b) files)
               return ()

readf :: MonadPCF m => String -> m String
readf filename = liftIO $ catch (readFile filename)
                        (\e -> do let err = show (e :: IOException)
                                  hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                                  return "")

typecheck :: MonadPCF m => String -> m ()
typecheck file = 
  do let filename = reverse(dropWhile isSpace (reverse file)) 
     x <- readf filename
     p <- parseIO filename program x
     sdecls <- mapM desugarDecl2 p
     let decls = [Decl p x ty (elab t) | Just (Decl p x ty t) <- sdecls]
     mapM tcDecl decls
     return ()

bytecompile :: MonadPCF m => String -> m ()
bytecompile file =
  do let filename = reverse(dropWhile isSpace (reverse file)) 
     x <- readf filename
     p <- parseIO filename program x
     sdecls <- mapM desugarDecl2 p
     let decls = [Decl p x ty (elab t) | Just (Decl p x ty t) <- sdecls]
     mapM tcDecl decls
     bc <- bytecompileModule decls
     printPCF (show bc)
     liftIO $ bcWrite bc (filename ++ ".bc")
     return ()

closConvert :: MonadPCF m => String -> m ()
closConvert file =
  do let filename = reverse(dropWhile isSpace (reverse file)) 
     x <- readf filename
     p <- parseIO filename program x
     sdecls <- mapM desugarDecl2 p
     let decls = [Decl p x ty (elab t) | Just (Decl p x ty t) <- sdecls]
     mapM tcDecl decls
     mapM_ (printPCF . show) (runCC decls)

compile :: MonadPCF m => Bool -> String -> m ()
compile opt file =
  do let filename = reverse(dropWhile isSpace (reverse file)) 
     x <- readf filename
     p <- parseIO filename program x
     sdecls <- mapM desugarDecl2 p
     let decls = [Decl p x ty (elab t) | Just (Decl p x ty t) <- sdecls]
     mapM tcDecl decls
     mapM addDecl decls
     optimized <- if opt then optimize decls else return decls
     mapM (printPCF . (\x -> declName x ++ " = " ++ (pp . declBody) x)) optimized
     let canon = runCanon (runCC optimized)
     liftIO $ TIO.writeFile "output.ll" (ppllvm (codegen canon))

gcc :: MonadPCF m => String -> m ()
gcc file =
  do let filename = reverse(dropWhile isSpace (reverse file)) 
     x <- readf filename
     p <- parseIO filename program x
     sdecls <- mapM desugarDecl2 p
     let decls = [Decl p x ty (elab t) | Just (Decl p x ty t) <- sdecls]
     mapM tcDecl decls
     let clos = runCC decls
     liftIO $ TIO.writeFile "output.c" (pack (ppC clos))

main' :: (MonadPCF m, MonadMask m) => [String] -> InputT m ()
main' args = do
        lift $ catchErrors $ compileFiles args
        s <- lift $ get
        when (inter s) $ liftIO $ putStrLn
          (  "Entorno interactivo para PCF0.\n"
          ++ "Escriba :? para recibir ayuda.")
        loop  
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c
                       maybe loop (flip when loop) b

data Mode = Interactive
          | Typecheck
          | Bytecompile
          | Run
          | ClosConvert
          | GCC
          | CompileLLVM Bool

-- | Parser de banderas
parseMode :: Parser Mode
parseMode =
        flag' Typecheck ( long "typecheck" <> short 't' <> help "Solo chequear tipos")
    <|> flag' Bytecompile (long "bytecompile" <> short 'c' <> help "Compilar a la BVM")
    <|> flag' Run (long "run" <> short 'r' <> help "Ejecutar bytecode en la BVM")
    <|> flag' ClosConvert (long "cconvert" <> short 'C' <> help "Convertir a clausuras")
    <|> flag Interactive Interactive ( long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva" )
    <|> flag' GCC (long "gcc" <> short 'g' <> help "Convertir a codigo C")
    <|> flag' CompileLLVM (long "compile" <> short 'X' <> help "Compilar a binario")
    <*> switch ( long "opt" <> short 'O' <> help "Aplicar optimizaciones" )

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode, [FilePath])
parseArgs = (,) <$> parseMode <*> many (argument str (metavar "FILES..."))
 
compileFiles ::  MonadPCF m => [String] -> m ()
compileFiles []     = return ()
compileFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile x
        compileFiles xs

compileFile ::  MonadPCF m => String -> m ()
compileFile f = do
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    decls <- parseIO filename program x
    mapM_ desugarDecl decls

parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

handleDecl :: MonadPCF m => Decl NTerm -> m ()
handleDecl (Decl p x ty t) = do
        let tt = elab t
        tcDecl (Decl p x ty tt)
        te <- evalCEK tt
        addDecl (Decl p x ty te)

desugarDecl :: MonadPCF m => SDecl -> m ()
desugarDecl (SDeclSyn p n ty)       = do mty <- lookupTySyn n
                                         case mty of
                                           Nothing -> do tty <- desugarTy ty
                                                         addTySyn n tty
                                           Just _  -> failPosPCF p $ n ++" ya está declarado"
desugarDecl (SDeclTm p n ty xs t)   = do tt <- desugar (SLam p xs t)
                                         tty <- desugarTy (foldTy xs ty)
                                         handleDecl (Decl p n tty tt)
desugarDecl (SDeclRec p f fty xs t) = do let fty' = (foldTy xs fty)
                                         tt <- desugar (SFix p f fty' xs t)
                                         tfty <- desugarTy fty'
                                         handleDecl (Decl p f tfty tt)

desugarDecl2 :: MonadPCF m => SDecl -> m (Maybe (Decl NTerm))
desugarDecl2 (SDeclSyn p n ty)       = do mty <- lookupTySyn n
                                          case mty of
                                             Nothing -> do tty <- desugarTy ty
                                                           addTySyn n tty
                                             Just _  -> failPosPCF p $ n ++" ya está declarado"
                                          return Nothing
desugarDecl2 (SDeclTm p n ty xs t)   = do tt <- desugar (SLam p xs t)
                                          tty <- desugarTy (foldTy xs ty)
                                          return (Just (Decl p n tty tt))
desugarDecl2 (SDeclRec p f fty xs t) = do let fty' = (foldTy xs fty)
                                          tt <- desugar (SFix p f fty' xs t)
                                          tfty <- desugarTy fty'
                                          return (Just (Decl p f tfty tt))

data Command = Compile CompileForm
             | Print String
             | Type String
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if isPrefixOf ":" x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive x))

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   Print          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadPCF m => Command  -> m Bool
handleCommand cmd = do
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printPCF (helpTxt commands) >> return True
       Browse ->  do  printPCF (unlines [ name | name <- reverse (nub (map declName glb)) ])
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> put (s {lfile=f}) >> compileFile f
                      return True
       Print e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

compilePhrase ::  MonadPCF m => String -> m ()
compilePhrase x =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of 
      Left d  -> desugarDecl d
      Right t -> handleTerm t

handleTerm ::  MonadPCF m => STerm -> m ()
handleTerm t = do
         t' <- desugar t
         let tt = elab t'
         s <- get
         ty <- tc tt (tyEnv s)
         te <- evalCEK tt
         printPCF (pp te ++ " : " ++ ppTy ty)

printPhrase   :: MonadPCF m => String -> m ()
printPhrase x =
  do
    x' <- parseIO "<interactive>" tm x
    x'' <- desugar x'
    let ex = elab x''
    t  <- case x' of 
           (SV p f) -> maybe ex id <$> lookupDecl f
           _       -> return ex  
    printPCF "NTerm:"
    printPCF (show x')
    printPCF "\nTerm:"
    printPCF (show t)

typeCheckPhrase :: MonadPCF m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" tm x
         t' <- desugar t
         let tt = elab t'
         s <- get
         ty <- tc tt (tyEnv s)
         printPCF (ppTy ty)