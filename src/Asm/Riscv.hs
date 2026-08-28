{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Asm.Riscv
  ( module Asm.Riscv
  ) where

import Asm.Asm (AsmPrinter(..))
import Codegen.Common (Register(regName))

import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Target.Riscv.Common (RiscVInst(..), Rv32Inst, Rv64Inst)

instance AsmPrinter Rv32Inst where
  emitInstAssembly :: Rv32Inst -> Builder
  emitInstAssembly = commonAsmEmitter

instance AsmPrinter Rv64Inst where
  emitInstAssembly :: Rv64Inst -> Builder
  emitInstAssembly (RV_Ld rs1 rs2 imm) = twoArgsInstHelper "ld" (getRegName rs1) ((getImm imm) <> "(" <> getRegName rs2 <> ")")
  emitInstAssembly (RV_Sd rs1 rs2 imm) = twoArgsInstHelper "sd" (getRegName rs1) ((getImm imm) <> "(" <> getRegName rs2 <> ")")
  emitInstAssembly (Rv_Mulw rd rs1 rs2) = threeArgsInstHelper "mulw" (getRegName rd) (getRegName rs1) (getRegName rs2)
  emitInstAssembly (Rv_Addw rd rs1 rs2) = threeArgsInstHelper "addw" (getRegName rd) (getRegName rs1) (getRegName rs2)
  emitInstAssembly (RV_Addiw rd rs imm) = threeArgsInstHelper "addiw" (getRegName rd) (getRegName rs) (getImm imm)
  emitInstAssembly (RV_Subw rd rs1 rs2) = threeArgsInstHelper "subw" (getRegName rd) (getRegName rs1) (getRegName rs2)
  emitInstAssembly inst = commonAsmEmitter inst

commonAsmEmitter :: RiscVInst a -> Builder
commonAsmEmitter (RV_Lw rs1 rs2 imm) = twoArgsInstHelper "lw" (getRegName rs1) ((getImm imm) <> "(" <> getRegName rs2 <> ")")
commonAsmEmitter (RV_Li rd imm) = twoArgsInstHelper "li" (getRegName rd) (getImm imm)
commonAsmEmitter (RV_Sw rs1 rs2 imm) = twoArgsInstHelper "sw" (getRegName rs1) ((getImm imm) <> "(" <> getRegName rs2 <> ")")
commonAsmEmitter (RV_Add rd rs1 rs2) = threeArgsInstHelper "add" (getRegName rd) (getRegName rs1) (getRegName rs2)
commonAsmEmitter (RV_Addi rd rs imm) = threeArgsInstHelper "addi" (getRegName rd) (getRegName rs) (getImm imm)
commonAsmEmitter (RV_Sub rd rs1 rs2) = threeArgsInstHelper "sub" (getRegName rd) (getRegName rs1) (getRegName rs2)
commonAsmEmitter (Rv_Mul rd rs1 rs2) = threeArgsInstHelper "mul" (getRegName rd) (getRegName rs1) (getRegName rs2)
commonAsmEmitter (Rv_Div rd rs1 rs2) = threeArgsInstHelper "div" (getRegName rd) (getRegName rs1) (getRegName rs2)
commonAsmEmitter (Rv_Rem rd rs1 rs2) = threeArgsInstHelper "rem" (getRegName rd) (getRegName rs1) (getRegName rs2)
commonAsmEmitter (RV_Slt rd rs1 rs2) = threeArgsInstHelper "slt" (getRegName rd) (getRegName rs1) (getRegName rs2)
commonAsmEmitter (RV_Slti rd rs1 imm) = threeArgsInstHelper "slti" (getRegName rd) (getRegName rs1) (getImm imm)
commonAsmEmitter (RV_Sltiu rd rs1 imm) = threeArgsInstHelper "sltiu" (getRegName rd) (getRegName rs1) (getImm imm)
commonAsmEmitter (RV_And rd rs1 rs2) = threeArgsInstHelper "and" (getRegName rd) (getRegName rs1) (getRegName rs2)
commonAsmEmitter (RV_Andi rd rs1 imm) = threeArgsInstHelper "andi" (getRegName rd) (getRegName rs1) (getImm imm)
commonAsmEmitter (RV_Or rd rs1 rs2) = threeArgsInstHelper "or" (getRegName rd) (getRegName rs1) (getRegName rs2)
commonAsmEmitter (RV_Ori rd rs1 imm) = threeArgsInstHelper "ori" (getRegName rd) (getRegName rs1) (getImm imm)
commonAsmEmitter (RV_Xor rd rs1 rs2) = threeArgsInstHelper "xor" (getRegName rd) (getRegName rs1) (getRegName rs2)
commonAsmEmitter (RV_Xori rd rs1 imm) = threeArgsInstHelper "xori" (getRegName rd) (getRegName rs1) (getImm imm)
commonAsmEmitter (Rv_Mv rd rs) = twoArgsInstHelper "mv" (getRegName rd) (getRegName rs)
commonAsmEmitter (RV_J label) = oneArgInstHelper "j" (fromString label)
commonAsmEmitter (RV_Jal rd label) = twoArgsInstHelper "jal" (getRegName rd) (fromString label)
commonAsmEmitter (RV_Call label) = oneArgInstHelper "call" (fromString label)
commonAsmEmitter (RV_Beq rs1 rs2 label) = threeArgsInstHelper "beq" (getRegName rs1) (getRegName rs2) (fromString label)
commonAsmEmitter (RV_Label label) = fromString label <> singleton ':'
commonAsmEmitter (RV_Ret) = "ret"
commonAsmEmitter (Rv_Ecall) = "ecall"
commonAsmEmitter _ = "Not implemented"

threeArgsInstHelper :: Builder -> Builder -> Builder -> Builder -> Builder
threeArgsInstHelper inst a1 a2 a3 = inst <> " " <> a1 <> ", " <> a2 <> ", " <> a3

twoArgsInstHelper :: Builder -> Builder -> Builder -> Builder
twoArgsInstHelper inst a1 a2 = inst <> " " <> a1 <> ", " <> a2

oneArgInstHelper :: Builder -> Builder -> Builder
oneArgInstHelper inst a1 = inst <> " " <> a1

getRegName :: Register -> Builder
getRegName = fromString . regName

getImm :: Show a => a -> Builder
getImm = fromString . show
