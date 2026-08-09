module Asm
  ( module Asm
  ) where

import Abi (Inst(InstRV), Register(regName), RiscVInst(RV_Add, RV_Addi, RV_Lw, Rv_Mulw, RV_Li, RV_Sbw, RV_Beq, RV_J, RV_Call, Rv_Mv))

emitInstAssembly :: Inst -> String
emitInstAssembly (InstRV inst) = emitInstRvAssembly inst
emitInstAssembly _ = "#TBD"

emitInstRvAssembly :: RiscVInst -> String
emitInstRvAssembly (RV_Lw rd rs imm) =
  "ld " ++ regName rd ++ ", " ++ show imm ++ "(" ++ regName rs ++ ")"
emitInstRvAssembly (RV_Addi rd rs imm) =
  "addiw " ++ regName rd ++ ", " ++ regName rs ++ ", " ++ show imm
emitInstRvAssembly (RV_Add rd rs1 rs2) =
  "addw " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly (Rv_Mulw rd rs1 rs2) =
  "mulw " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly (RV_Li rd imm) = 
  "li " ++ regName rd ++ ", " ++ show imm
emitInstRvAssembly (RV_Sbw rd rs1 rs2) =
  "subw " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly (RV_Beq rs1 rs2 target) = 
  "beq " ++ regName rs1 ++ ", " ++ regName rs2 ++ ", " ++ show target
emitInstRvAssembly (RV_J target) =
  "j " ++ show target
emitInstRvAssembly (RV_Call target) =
  "call " ++ show target
emitInstRvAssembly (Rv_Mv rd rs) =
  "mv " ++ regName rd ++ ", " ++ regName rs
emitInstRvAssembly _ = "#TBD"

emitAssembly :: [Inst] -> String
emitAssembly insts = unlines (map emitInstAssembly insts)
