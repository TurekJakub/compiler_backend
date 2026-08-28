module Demo where

import Asm.Asm (printAssembly)
import Asm.Riscv ()
import Codegen.Codegen (codegen)
import Ir
  ( Definition(Function)
  , FuncTypeSignature(FuncTypeSignature, argTypes, returnType)
  , FunctionDef(..)
  , FunctionPrototype(FunctionPrototype, name, signature)
  , IrToken(Add, Branch, ConditionalBranch, FunctionCall, GetLocal, Gt, IrLiteral, Label, Mul, Not, PrintInt, SetLocal, Sub)
  , IrType(IntType)
  , Literal(IntLiteral)
  , Program
  )
import Target.Riscv.Common (Rv32Inst)
import Target.Riscv.Rv32 ()

factorialDef :: FunctionDef
factorialDef =
  FunctionDef
    { prototype = FunctionPrototype {name = "factorial", signature = FuncTypeSignature {argTypes = [IntType], returnType = IntType}}
    , body =
        [ IrLiteral (IntLiteral 1)
        , SetLocal "result"
        , GetLocal "arg0"
        , SetLocal "n"
        , Label "loop_start"
        , GetLocal "n"
        , IrLiteral (IntLiteral 1)
        , Gt
        , ConditionalBranch "loop_end"
        , GetLocal "result"
        , GetLocal "n"
        , Mul
        , SetLocal "result"
        , GetLocal "n"
        , IrLiteral (IntLiteral 1)
        , Sub
        , SetLocal "n"
        , Branch "loop_start"
        , Label "loop_end"
        , GetLocal "result"
        ]
    }

fibsDef :: FunctionDef
fibsDef =
  FunctionDef
    { prototype = FunctionPrototype {name = "fib", signature = FuncTypeSignature {argTypes = [IntType], returnType = IntType}}
    , body =
        [ GetLocal "arg0"
        , SetLocal "n"
        , GetLocal "n"
        , IrLiteral (IntLiteral 1)
        , Gt
        , Not
        , ConditionalBranch "base_case_false"
        , GetLocal "n"
        , Branch "end_fib"
        , Label "base_case_false"
        , GetLocal "n"
        , IrLiteral (IntLiteral 1)
        , Sub
        , FunctionCall "fib"
        , SetLocal "fib_n_minus_1"
        , GetLocal "n"
        , IrLiteral (IntLiteral 2)
        , Sub
        , FunctionCall "fib"
        , SetLocal "fib_n_minus_2"
        , GetLocal "fib_n_minus_1"
        , GetLocal "fib_n_minus_2"
        , Add
        , Label "end_fib"
        ]
    }

factorialMainDef :: FunctionDef
factorialMainDef =
  FunctionDef
    { prototype = FunctionPrototype {name = "main", signature = FuncTypeSignature {argTypes = [], returnType = IntType}}
    , body = [IrLiteral (IntLiteral 5), FunctionCall "factorial", PrintInt]
    }

fibMainDef :: FunctionDef
fibMainDef =
  FunctionDef
    { prototype = FunctionPrototype {name = "main", signature = FuncTypeSignature {argTypes = [], returnType = IntType}}
    , body = [IrLiteral (IntLiteral 5), FunctionCall "fib", PrintInt]
    }

-- Demo program that compute 5!
factorialDemoProgram :: Program
factorialDemoProgram = [Function factorialDef, Function factorialMainDef]

-- demo program that computes fifth Fibonacci number
fibsDemoProgram :: [Definition]
fibsDemoProgram = [Function fibsDef, Function fibMainDef]

runDemo :: IO ()
runDemo = do
  let fibResult = codegen @Rv32Inst fibsDemoProgram
      factorialResult = codegen @Rv32Inst factorialDemoProgram
  putStrLn "Running Codegen Demo..."
  putStrLn "--- Generated RISC-V assembly for Fibonacci numbers example ---"
  printAssembly @Rv32Inst fibResult
  putStrLn "--- Generated RISC-V assembly for factorial example ---"
  printAssembly @Rv32Inst factorialResult
