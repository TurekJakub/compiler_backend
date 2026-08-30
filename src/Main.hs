{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

import Target.Riscv.Common (Rv32Inst, Rv64Inst)

import Target.Riscv.Rv32 ()
import Target.Riscv.Rv64 ()

import Asm.Asm (AsmPrinter, printAssembly)
import Codegen.Codegen (codegen)
import Options.Applicative

import Target.Target (InstSelector)

data ArchOpt
  = Rv32
  | Rv64
  deriving (Show, Read, Eq)

data CliOptions = CliOptions
  { demoSource :: Maybe String
  , outputPath :: String
  , arch :: ArchOpt
  }

testInput :: Program
testInput =
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

optionsParser :: Parser CliOptions
optionsParser =
  CliOptions
    <$> optional (strOption (long "demo-file" <> short 'd' <> metavar "DEMO_file" <> help "Demo example source file"))
    <*> strOption
          (long "output" <> short 'o' <> metavar "OUTPUT_PATH" <> help "Emitted assembly output file" <> value "out.s" <> showDefault)
    <*> option auto (long "target-arch" <> short 'a' <> metavar "RV32|RV64" <> value Rv32 <> showDefault <> help "Target CPU architecture")

main :: IO ()
main = do
  let opts = info (optionsParser <**> helper) (fullDesc <> progDesc "Simple RISC-V codegen demo")
  parsedOpts <- execParser opts
  let demoExample = demoSource parsedOpts
  case (arch parsedOpts) of
    Rv32 -> execute @Rv32Inst demoExample (outputPath parsedOpts)
    Rv64 -> execute @Rv64Inst demoExample (outputPath parsedOpts)
  where
    execute ::
         forall arch. (AsmPrinter arch, InstSelector arch)
      => Maybe String
      -> String
      -> IO ()
    execute demoSource outputPath =
      case demoSource of
        Just s -> runDemo @arch s outputPath
        Nothing -> do
          let codegenResult = codegen @arch testInput
          putStrLn "--- Generated RISC-V assembly ---"
          printAssembly @arch codegenResult
