{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

module Target.Riscv.Common
  ( module Target.Riscv.Common
  ) where

import Control.Monad.State ( State )
import GHC.Generics (Generic)
import Ir (LabelName, Literal(IntLiteral))
import Codegen.Common
  ( CodegenState
  , Immediate
  , Register(Register)
  , RegisterType(GeneralPurpose)
  , VStackItem(Immediate, Reg)
  , freeRegister
  , invalidateCacheLine
  )
import Optics ( (^.) )
import Target.Target (InstSelector(forceToReg), is12BitsImm, Arch(..))

type Rv64Inst = RiscVInst 'RV64
type Rv32Inst = RiscVInst 'RV32

data RiscVInst (a :: Arch) where
  RV_Lw :: Register -> Register -> Immediate -> RiscVInst a
  RV_Sw :: Register -> Register -> Immediate -> RiscVInst a
  RV_Li :: Register -> Immediate -> RiscVInst a
  RV_Add :: Register -> Register -> Register -> RiscVInst a
  RV_Addi :: Register -> Register -> Immediate -> RiscVInst a
  RV_Sub :: Register -> Register -> Register -> RiscVInst a
  Rv_Mul :: Register -> Register -> Register -> RiscVInst a
  RV_Slt :: Register -> Register -> Register -> RiscVInst a
  RV_J :: LabelName -> RiscVInst a
  RV_Label :: LabelName -> RiscVInst a
  RV_Beq :: Register -> Register -> LabelName -> RiscVInst a
  RV_Jal :: Register -> LabelName -> RiscVInst a
  RV_Call :: LabelName -> RiscVInst a
  RV_Ret :: RiscVInst a
  Rv_Mv :: Register -> Register -> RiscVInst a
  RV_Ld :: Register -> Register -> Immediate -> RiscVInst 'RV64
  RV_Sd :: Register -> Register -> Immediate -> RiscVInst 'RV64
  Rv_Addw :: Register -> Register -> Register -> RiscVInst 'RV64
  RV_Addiw :: Register -> Register -> Immediate -> RiscVInst 'RV64
  RV_Subw :: Register -> Register -> Register -> RiscVInst 'RV64
  Rv_Mulw :: Register -> Register -> Register -> RiscVInst 'RV64

rvZeroRegister :: Register
rvZeroRegister = Register "zero" GeneralPurpose

type ImmRegBinOpCodegen (a :: Arch)  = Register -> Register -> Immediate -> State (CodegenState (RiscVInst a)) ()
data BinOpDefinition (a :: Arch) = BinOpDefinition
  { regToRegCodegen :: Register -> Register -> Register -> State (CodegenState(RiscVInst a))()
  , immToRegCodegen :: Maybe (ImmRegBinOpCodegen a)
  , regToImmCodegen :: Maybe (ImmRegBinOpCodegen a)
  } deriving (Generic)

binCodgenOpHelper ::
     InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> (BinOpDefinition a)
  -> State (CodegenState (RiscVInst a)) VStackItem
binCodgenOpHelper lhs rhs binOpDef =
  case (lhs, rhs) of
    (Reg r1, Reg r2) -> do
      (binOpDef ^. #regToRegCodegen) r1 r1 r2
      let res = Reg r1
      invalidateCacheLine res
      freeRegister r2
      pure res
    (Reg r1, Immediate (IntLiteral i1))
      | Just handler <- binOpDef ^. #immToRegCodegen -> do
        handleImmediate handler i1 r1
    (Immediate (IntLiteral i1), Reg r1)
      | Just handler <- binOpDef ^. #regToImmCodegen -> do
        handleImmediate handler i1 r1
    _ -> genericCodgen
  where
    genericCodgen = do
      lhsTmp <- forceToReg lhs
      rhsTmp <- forceToReg rhs
      (binOpDef ^. #regToRegCodegen) rhsTmp lhsTmp rhsTmp
      freeRegister lhsTmp
      let res = Reg rhsTmp
      invalidateCacheLine res
      pure res
    handleImmediate codgen i1 r1 =
      if is12BitsImm i1
        then do
          codgen r1 r1 i1
          let res = Reg r1
          invalidateCacheLine res
          pure res
        else genericCodgen
