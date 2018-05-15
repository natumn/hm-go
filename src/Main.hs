module Main
where

import           Parser

import           Control.Monad.Trans
import           System.Console.Haskeline
import           System.Environment
import qualified LLVM.AST                      as AST

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
    let res = parseToplevel source
    case res of
        Left  err -> print err >> return Nothing
        Right ex  -> do
            ast <- codegen modo ex
            return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop mod = do
        minput <- getInputLine "hli> "
        case minput of
            Nothing    -> outputStrLn "Goodbye."
            Just input -> do
                modo <- liftIO $ process mod Input
                case modn of
                    Nothing   -> loop mod
                    Just modo -> loop modn

main :: IO ()
main = do
    args <- getArgs
    case args of
        []      -> repl
        [fname] -> processFile fname >> return ()

