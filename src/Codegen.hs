-- module Codegen where

-- import Data.Word
-- import Data.String
-- import Data.List
-- import Data.Function
-- import qualified Data.Map as Map

-- import Control.Monad.State
-- import Control.Applicative

-- import LLVM.AST
-- import LLVM.AST.Global
-- import qualified LLVM.AST as AST

-- import qualified LLVM.AST.Linkage as L
-- import qualified LLVM.AST.Constant as C
-- import qualified LLVM.AST.Attribute as A
-- import qualified LLVM.AST.CallingConvention as CC
-- import qualified LLVM.AST.FloatingPointPredicate as FP

-- type SymbolTable = [(String, Operand)]

-- data CodegenState
--   = CodegenState {
--     currentBlock :: Name                     -- Name of the active block to append to
--   , blocks       :: Map.Map Name BlockState  -- Blocks for function
--   , symtab       :: SymbolTable              -- Function scope symbol table
--   , blockCount   :: Int                      -- Count of basic blocks
--   , count        :: Word                     -- Count of unnamed instructions
--   , names        :: Names                    -- Name Supply
--   } deriving Show

-- data BlockState
--   = BlockState {
--     idx   :: Int                            -- Block index
--   , stack :: [Named Instruction]            -- Stack of instructions
--   , term  :: Maybe (Named Terminator)       -- Block terminator
--   } deriving Show