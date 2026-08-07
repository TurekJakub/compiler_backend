module Asm
  ( module Asm
  ) where

import Abi (Inst(InstRV), Register(regName), RiscVInst(RV_Add, RV_Addi, RV_Lw))

emitInstAssembly :: Inst -> String
emitInstAssembly (InstRV inst) = emitInstRvAssembly inst
emitInstAssembly _ = "#TBD"

emitInstRvAssembly :: RiscVInst -> String
emitInstRvAssembly (RV_Lw rd rs imm) =
  "ld " ++ regName rd ++ ", " ++ show imm ++ "(" ++ regName rs ++ ")"
emitInstRvAssembly (RV_Addi rd rs imm) =
  "addi " ++ regName rd ++ ", " ++ regName rs ++ ", " ++ show imm
emitInstRvAssembly (RV_Add rd rs1 rs2) =
  "add " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly _ = "#TBD"

emitAssembly :: [Inst] -> String
emitAssembly insts = unlines (map emitInstAssembly insts)
