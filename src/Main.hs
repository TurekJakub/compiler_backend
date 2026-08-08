module Main
  ( main
  ) where

import Abi (Register(..), RegisterType(GeneralPurpose))
import Asm (emitAssembly)
import Codegen (CodegenState(..), codgen)
import qualified Data.Map as Map
import Ir (IrToken(Add, IrLiteral, Peek, Mul, Sub), Literal(IntLiteral))

testInput :: [IrToken]
testInput = [Peek 8, IrLiteral (IntLiteral 42), Add, IrLiteral (IntLiteral 7), Sub, IrLiteral (IntLiteral 42), Mul]

testFreeRegisters :: [Register]
testFreeRegisters =
  map (\n -> Register ("t" ++ show n) GeneralPurpose) ([0 .. 3] :: [Int])

initialState :: CodegenState
initialState =
  CodegenState
    { virtualStack = []
    , freeRegisters = testFreeRegisters
    , cache = Map.empty
    , emittedCode = []
    }

main :: IO ()
main = do
  let codegenResult = codgen testInput initialState
  putStrLn "--- Generated RISC-V assembly ---"
  putStr (emitAssembly codegenResult)
