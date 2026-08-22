module Main
  ( main
  ) where

import Asm.Asm (printAssembly)
import Asm.Riscv ()
import Codegen.Codegen (codegen)
import Ir
  ( FuncTypeSignature(FuncTypeSignature, argTypes, returnType)
  , FunctionDef(FunctionDef, body, prototype)
  , FunctionPrototype(FunctionPrototype, name, signature)
  , IrToken(Add, Branch, ConditionalBranch, Eq, FunctionCall, GetLocal, Gte, IrLiteral,
            Label, Mul, Not, Sub)
  , IrType(IntType, VoidType)
  , Literal(IntLiteral)
  , Program
  )
import Target.Riscv.Common (Rv32Inst, Rv64Inst)
import Target.Riscv.Rv32 ()
import Target.Riscv.Rv64 ()

testInput :: Program
testInput =
  [ FunctionDef
      { prototype =
          FunctionPrototype
            { name = "main"
            , signature =
                FuncTypeSignature {returnType = VoidType, argTypes = []}
            }
      , body =
          [ IrLiteral $ IntLiteral 0
          , IrLiteral $ IntLiteral 1
          , IrLiteral $ IntLiteral 2
          , IrLiteral $ IntLiteral 3
          , IrLiteral $ IntLiteral 4
          , IrLiteral $ IntLiteral 5
          , IrLiteral $ IntLiteral 6
          , IrLiteral $ IntLiteral 7
          , IrLiteral $ IntLiteral 8
          , FunctionCall "test"
          ]
      }
  , FunctionDef
      { prototype =
          FunctionPrototype
            { name = "test"
            , signature =
                FuncTypeSignature
                  { returnType = IntType
                  , argTypes =
                      [ IntType
                      , IntType
                      , IntType
                      , IntType
                      , IntType
                      , IntType
                      , IntType
                      , IntType
                      , IntType
                      ]
                  }
            }
      , body =
          [ GetLocal "arg8" {-
          , IrLiteral (IntLiteral 42)
          , Add
          , IrLiteral (IntLiteral 7)
          , Sub
          , IrLiteral (IntLiteral 42)
          , Mul-}
          , IrLiteral $ IntLiteral 8
          , Eq
          , Not
          , ConditionalBranch "UwU"
          , IrLiteral (IntLiteral 10)
          , Branch "OwO"
          , Label "UwU"
          , IrLiteral (IntLiteral 20)
          , Label "OwO"
          ]
      }
  ]

main :: IO ()
main = do
  let codegenResult = codegen @Rv32Inst testInput
  putStrLn "--- Generated RISC-V assembly ---"
  printAssembly codegenResult
