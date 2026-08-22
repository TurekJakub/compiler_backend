{-# OPTIONS_GHC -Wno-orphans #-}

module Target.Riscv.Rv32 where

import Ir
import Codegen.Common

import Control.Monad.State
import Data.Map (Map)

import Target.Riscv.Common
import Target.Target (InstSelector (..), RegisterAllocator(initialRegisterPool))

instance InstSelector Rv32Inst where
  initCodegen :: Int -> Int -> Map String FuncTypeSignature -> CodegenState Rv32Inst
  initCodegen = rvInitCodegen @Rv32Inst @Rv32Inst

  codegenAdd :: VStackItem-> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenAdd = rvCodegenAdd

  codegenSub :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenSub = rvCodegenSub

  codegenMul :: VStackItem-> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenMul = rvCodegenMul

  codegenBranchIfZero :: VStackItem -> String -> State (CodegenState Rv32Inst) ()
  codegenBranchIfZero = rvCodegenBranchIfZero

  loadImmediate :: Immediate -> Register -> State (CodegenState Rv32Inst) ()
  loadImmediate = rvLoadImmediate
  emitLoad :: Register -> Register -> Immediate -> Rv32Inst
  emitLoad target src offset = RV_Lw target src offset
  emitStore :: Register -> Register -> Immediate -> Rv32Inst
  emitStore src targetAddr offset = RV_Sw src targetAddr offset
  emitAddi :: Register -> Register -> Immediate -> Rv32Inst
  emitAddi = rvEmitAddi
  emitMove :: Register -> Register -> State (CodegenState Rv32Inst) ()
  emitMove = rvEmitMove
  emitCall :: String -> IrType -> State (CodegenState Rv32Inst) [VStackItem]
  emitCall =rvEmitCall
  emitLabel :: String -> Rv32Inst
  emitLabel =rvEmitLabel
  emitJump :: String -> Rv32Inst
  emitJump = rvEmitJump
  emitBranchIfEqual :: Register -> Register -> String -> Rv32Inst
  emitBranchIfEqual = rvEmitBranchIfEqual
  emitFuncProlog :: FunctionDef -> Int -> [Rv32Inst]
  emitFuncProlog = rvEmitFuncProlog
  emitFuncEpilog :: Int -> [Rv32Inst]
  emitFuncEpilog = rvEmitFuncEpilog
  spRegister :: Register
  spRegister = rvSpRegister
  raRegister :: Register
  raRegister =rvRaRegister
  spAlignment :: Int
  spAlignment =rvSpAlignment
  returnValueRegisters :: [Register]
  returnValueRegisters = rvReturnValueRegisters
  extraFrameSlotsCount :: Int
  extraFrameSlotsCount = rvExtraFrameSlotsCount
  funcArgumentsRegistersCount :: Int
  funcArgumentsRegistersCount = rvFuncArgumentsRegistersCount
  registerSize :: Int
  registerSize = 4


instance RegisterAllocator Rv32Inst where
  initialRegisterPool :: [Register]
  initialRegisterPool = rvInitialRegisterPool