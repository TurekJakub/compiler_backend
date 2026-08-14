module Main
  ( main
  ) where

import Asm (emitAssembly)
import Codegen ( codegen)
import Ir (IrToken(Add, IrLiteral, Mul, Sub, SetLocal, GetLocal, FunctionCall), Literal(IntLiteral), FuncTypeSignature (FuncTypeSignature, argTypes, returnType), IrType (VoidType, IntType), Program, FunctionDef (FunctionDef, body, prototype), FunctionPrototype (FunctionPrototype, name, signature))

testInput :: Program
testInput = [FunctionDef {prototype=FunctionPrototype{name="main",signature=FuncTypeSignature {returnType=VoidType, argTypes=[]}}, body=[IrLiteral $ IntLiteral 7, SetLocal "x", GetLocal "x", FunctionCall "test"]},
  FunctionDef {prototype=FunctionPrototype{name="test",signature=FuncTypeSignature {returnType=IntType, argTypes=[IntType]}}, body=[GetLocal "arg0",  IrLiteral (IntLiteral 42), Add, IrLiteral (IntLiteral 7), Sub, IrLiteral (IntLiteral 42), Mul]} ]

main :: IO ()
main = do
  let codegenResult = codegen testInput 
  putStrLn "--- Generated RISC-V assembly ---"
  putStr (emitAssembly codegenResult)
