{-# OPTIONS_GHC -Wno-orphans #-}

module Target.Riscv.Rv32 where

import Codegen.Common
import Ir

import Control.Monad.State
import Data.Map (Map)

import Target.Riscv.Common
import Target.Target (InstSelector(..), RegisterAllocator(..))

instance InstSelector Rv32Inst where
  initCodegen :: Int -> Int -> Map String FuncTypeSignature -> CodegenState Rv32Inst
  initCodegen = rvInitCodegen @Rv32Inst @Rv32Inst
  codegenAdd :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenAdd = rvCodegenAdd
  codegenSub :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenSub = rvCodegenSub
  codegenMul :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenMul = rvCodegenMul
  codegenDiv :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenDiv = rvCodegenDiv
  codegenMod :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenMod = rvCodegenMod
  codegenLt :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenLt = rvCodegenLt
  codegenLte :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenLte = rvCodegenLte
  codegenGt :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenGt = rvCodegenGt
  codegenGte :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenGte = rvCodegenGte
  codegenEq :: VStackItem -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenEq = rvCodegenEq
  codegenNot :: VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenNot = rvCodegenLogicalNot
  codegenBranchIfZero :: VStackItem -> String -> State (CodegenState Rv32Inst) ()
  codegenBranchIfZero = rvCodegenBranchIfZero
  codegenGetLocalAddr :: Int -> State (CodegenState Rv32Inst) VStackItem
  codegenGetLocalAddr = rvCodegenGetLocalAddr
  codegenLoad :: IrType -> Int -> VStackItem -> State (CodegenState Rv32Inst) VStackItem
  codegenLoad = rvCodegenLoad
  codegenStore :: IrType -> Int -> VStackItem -> VStackItem -> State (CodegenState Rv32Inst) ()
  codegenStore = rvCodegenStore
  loadImmediate :: Immediate -> Register -> State (CodegenState Rv32Inst) ()
  loadImmediate = rvLoadImmediate
  emitLoad :: Register -> Register -> Immediate -> Rv32Inst
  emitLoad target src offset = RV_Lw target src offset
  codegenSyscall :: Int -> State (CodegenState Rv32Inst) ()
  codegenSyscall = rvCodegenSyscall
  codeGenPrintInt :: VStackItem -> State (CodegenState Rv32Inst) ()
  codeGenPrintInt = rvCodegenPrintInt
  emitStore :: Register -> Register -> Immediate -> Rv32Inst
  emitStore src targetAddr offset = RV_Sw src targetAddr offset
  emitAddi :: Register -> Register -> Immediate -> Rv32Inst
  emitAddi = rvEmitAddi
  emitMove :: Register -> Register -> State (CodegenState Rv32Inst) ()
  emitMove = rvEmitMove
  emitCall :: String -> IrType -> State (CodegenState Rv32Inst) [VStackItem]
  emitCall = rvEmitCall
  emitLabel :: String -> Rv32Inst
  emitLabel = rvEmitLabel
  emitJump :: String -> Rv32Inst
  emitJump = rvEmitJump
  emitBranchIfEqual :: Register -> Register -> String -> Rv32Inst
  emitBranchIfEqual = rvEmitBranchIfEqual
  emitFuncProlog :: FunctionDef -> Int -> [Rv32Inst]
  emitFuncProlog = rvEmitFuncProlog
  emitFuncEpilog :: Int -> String -> [Rv32Inst]
  emitFuncEpilog = rvEmitFuncEpilog
  spRegister :: Register
  spRegister = rvSpRegister
  raRegister :: Register
  raRegister = rvRaRegister
  spAlignment :: Int
  spAlignment = rvSpAlignment
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
  callerSavedRegisters :: [Register]
  callerSavedRegisters = rvCallerSavedRegisters
