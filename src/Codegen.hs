{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

-- module level
newtype LLVM a = LLVM ( State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadSate, AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name = Name label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType = retty
  , basicBlocks = body
}

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name = Name label
  , linkage = L.External
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType = retty
  , basicBlocks = []
  }

-- type
double :: Type
double = FloatingPointType as IEEE

-- codegeneration state
type SymbolTable [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name
    , blocks     :: Map.Map Name BlockState
    , symtab     :: SymbolTable
    , blockCount :: Int
    , count      :: Word
    , names      :: Names
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int
  , stack :: [Named Instruction]
  , term  :: Maybe (Named Terminator)
  } deriving Show

-- Codegeneration Operations

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadSate, CodegenState )
