module Asm
  ( module Asm
  ) where

import Abi
  ( Inst(InstRV)
  , Register(regName)
  , RiscVInst(RV_Add, RV_Addi, RV_Addiw, RV_Beq, RV_Call, RV_J, RV_Label, RV_Ld,
              RV_Li, RV_Lw, RV_Ret, RV_Sd, RV_Sub, RV_Subw, Rv_Addw, Rv_Mul, Rv_Mulw,
              Rv_Mv)
  )

emitInstAssembly :: Inst -> String
emitInstAssembly (InstRV inst) = emitInstRvAssembly inst
emitInstAssembly _ = "#TBD"

emitInstRvAssembly :: RiscVInst -> String
emitInstRvAssembly (RV_Lw rd rs imm) =
  "lw " ++ regName rd ++ ", " ++ show imm ++ "(" ++ regName rs ++ ")"
emitInstRvAssembly (RV_Ld rd rs imm) =
  "ld " ++ regName rd ++ ", " ++ show imm ++ "(" ++ regName rs ++ ")"
emitInstRvAssembly (RV_Addiw rd rs imm) =
  "addiw " ++ regName rd ++ ", " ++ regName rs ++ ", " ++ show imm
emitInstRvAssembly (RV_Addi rd rs imm) =
  "addi " ++ regName rd ++ ", " ++ regName rs ++ ", " ++ show imm
emitInstRvAssembly (RV_Add rd rs1 rs2) =
  "add " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly (Rv_Addw rd rs1 rs2) =
  "addw " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly (Rv_Mulw rd rs1 rs2) =
  "mulw " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly (Rv_Mul rd rs1 rs2) =
  "mul " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly (RV_Li rd imm) = "li " ++ regName rd ++ ", " ++ show imm
emitInstRvAssembly (RV_Subw rd rs1 rs2) =
  "subw " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly (RV_Sub rd rs1 rs2) =
  "sub " ++ regName rd ++ ", " ++ regName rs1 ++ ", " ++ regName rs2
emitInstRvAssembly (RV_Beq rs1 rs2 target) =
  "beq " ++ regName rs1 ++ ", " ++ regName rs2 ++ ", " ++ target
emitInstRvAssembly (RV_J target) = "j " ++ target
emitInstRvAssembly (RV_Call target) = "call " ++ target
emitInstRvAssembly (Rv_Mv rd rs) = "mv " ++ regName rd ++ ", " ++ regName rs
emitInstRvAssembly (RV_Label label) = label ++ ":"
emitInstRvAssembly RV_Ret = "ret"
emitInstRvAssembly (RV_Sd rs1 rs2 imm) =
  "sd " ++ regName rs1 ++ ", " ++ show imm ++ "(" ++ regName rs2 ++ ")"
emitInstRvAssembly _ = "#TBD"

emitAssembly :: [Inst] -> String
emitAssembly insts = unlines (map emitInstAssembly insts)
