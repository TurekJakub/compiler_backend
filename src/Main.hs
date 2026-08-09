module Main
  ( main
  ) where

import Asm (emitAssembly)
import Codegen ( codgen)
import Ir (IrToken(Add, IrLiteral, Peek, Mul, Sub), Literal(IntLiteral), FuncTypeSignature (FuncTypeSignature, argTypes, returnType), IrType (VoidType), Program, FunctionDef (FunctionDef, body, prototype), FunctionPrototype (FunctionPrototype, name, signature))

testInput :: Program
testInput = [FunctionDef {prototype=FunctionPrototype{name="main",signature=FuncTypeSignature {returnType=VoidType, argTypes=[]}}, body=[Peek 8, IrLiteral (IntLiteral 42), Add, IrLiteral (IntLiteral 7), Sub, IrLiteral (IntLiteral 42), Mul]},
  FunctionDef {prototype=FunctionPrototype{name="test",signature=FuncTypeSignature {returnType=VoidType, argTypes=[]}}, body=[Peek 8, IrLiteral (IntLiteral 42), Add, IrLiteral (IntLiteral 7), Sub, IrLiteral (IntLiteral 42), Mul]} ]

main :: IO ()
main = do
  let codegenResult = codgen testInput 
  putStrLn "--- Generated RISC-V assembly ---"
  putStr (emitAssembly codegenResult)
