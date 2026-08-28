module Main
  ( main
  ) where

import Asm.Riscv ()
import Demo (runDemo)
import Ir
  ( Definition(Function)
  , FuncTypeSignature(FuncTypeSignature, argTypes, returnType)
  , FunctionDef(FunctionDef, body, prototype)
  , FunctionPrototype(FunctionPrototype, name, signature)
  , IrToken(FunctionCall, GetLocal, GetLocalAddr, IrLiteral, SetLocal, Store)
  , IrType(IntType, VoidType)
  , Literal(IntLiteral)
  , Program
  )
import Target.Riscv.Rv32 ()
import Target.Riscv.Rv64 ()

_testInput :: Program
_testInput =
  [ Function
      $ FunctionDef
          { prototype = FunctionPrototype {name = "main", signature = FuncTypeSignature {returnType = IntType, argTypes = []}}
      , body =
          [ IrLiteral $ IntLiteral 42
          , SetLocal "x"
          , IrLiteral $ IntLiteral 1
          , IrLiteral $ IntLiteral 2
          , IrLiteral $ IntLiteral 3
          , IrLiteral $ IntLiteral 4
          , IrLiteral $ IntLiteral 5
          , IrLiteral $ IntLiteral 6
          , IrLiteral $ IntLiteral 7
          , IrLiteral $ IntLiteral 8
          , GetLocalAddr "x"
          , FunctionCall "test"
          , GetLocal "x"
          ]
      }
  , Function
      $ FunctionDef
      { prototype =
          FunctionPrototype
            { name = "test"
            , signature =
                FuncTypeSignature
                  { returnType = VoidType -- IntType
                      , argTypes = [IntType, IntType, IntType, IntType, IntType, IntType, IntType, IntType, IntType]
                  }
            }
      , body =
          [ IrLiteral $ IntLiteral 84
          , GetLocal "arg8"
          , Store IntType 0
          {-
          ,  GetLocal "arg8"
          , IrLiteral (IntLiteral 42)
          , Add
          , IrLiteral (IntLiteral 7)
          , Sub
          , IrLiteral (IntLiteral 42)
          , Mul
          , IrLiteral $ IntLiteral 8
          , Eq
          , Not
          , ConditionalBranch "UwU"
          , IrLiteral $ IntLiteral 10
          , Branch "OwO"
          , Label "UwU"
          , IrLiteral (IntLiteral 20)
          , Label "OwO"
          -}
          ]
      }
  ]

main :: IO ()
main = do
  let codegenResult = codegen @Rv32Inst testInput
  putStrLn "--- Generated RISC-V assembly ---"
  printAssembly codegenResult
