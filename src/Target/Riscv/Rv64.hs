{-# OPTIONS_GHC -Wno-orphans #-}

module Target.Riscv.Rv64
  ( 
  ) where

import Codegen.Common
import Ir

import Control.Monad.State
import Data.Map (Map)

import Target.Riscv.Common
import Target.Target (InstSelector(..), RegisterAllocator(..))

instance InstSelector Rv64Inst where
  initCodegen :: Int -> Int -> Map String FuncTypeSignature -> CodegenState Rv64Inst
  initCodegen = rvInitCodegen @Rv64Inst @Rv64Inst
  codegenAdd :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenAdd = rvCodegenAdd
  codegenSub :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenSub = rvCodegenSub
  codegenMul :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenMul = rvCodegenMul
  codegenDiv :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenDiv = rvCodegenDiv
  codegenMod :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenMod = rvCodegenMod
  codegenLt :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenLt = rvCodegenLt
  codegenLte :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenLte = rvCodegenLte
  codegenGt :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenGt = rvCodegenGt
  codegenGte :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenGte = rvCodegenGte
  codegenEq :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenEq = rvCodegenEq
  codegenNot :: VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenNot = rvCodegenLogicalNot
  codegenBranchIfZero :: VStackItem -> String -> State (CodegenState Rv64Inst) ()
  codegenBranchIfZero = rvCodegenBranchIfZero
  loadImmediate :: Immediate -> Register -> State (CodegenState Rv64Inst) ()
  codegenGetLocalAddr :: Int -> State (CodegenState Rv64Inst) VStackItem
  codegenGetLocalAddr = rvCodegenGetLocalAddr
  codegenLoad :: IrType -> Int -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenLoad = rvCodegenLoad
  codegenStore :: IrType -> Int -> VStackItem -> VStackItem -> State (CodegenState Rv64Inst) ()
  codegenStore = rvCodegenStore
  loadImmediate = rvLoadImmediate
  emitLoad :: Register -> Register -> Immediate -> Rv64Inst
  emitLoad target src offset = RV_Ld target src offset
  emitStore :: Register -> Register -> Immediate -> Rv64Inst
  emitStore src targetAddr offset = RV_Sd src targetAddr offset
  codegenSyscall :: Int -> State (CodegenState Rv64Inst) ()
  codegenSyscall = rvCodegenSyscall
  codeGenPrintInt :: VStackItem -> State (CodegenState Rv64Inst) ()
  codeGenPrintInt = rvCodegenPrintInt
  emitAddi :: Register -> Register -> Immediate -> Rv64Inst
  emitAddi = rvEmitAddi
  emitMove :: Register -> Register -> State (CodegenState Rv64Inst) ()
  emitMove = rvEmitMove
  emitCall :: String -> IrType -> State (CodegenState Rv64Inst) [VStackItem]
  emitCall = rvEmitCall
  emitLabel :: String -> Rv64Inst
  emitLabel = rvEmitLabel
  emitJump :: String -> Rv64Inst
  emitJump = rvEmitJump
  emitBranchIfEqual :: Register -> Register -> String -> Rv64Inst
  emitBranchIfEqual = rvEmitBranchIfEqual
  emitFuncProlog :: FunctionDef -> Int -> [Rv64Inst]
  emitFuncProlog = rvEmitFuncProlog
  emitFuncEpilog :: Int -> String -> [Rv64Inst]
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
  registerSize = 8

instance RegisterAllocator Rv64Inst where
  initialRegisterPool :: [Register]
  initialRegisterPool = rvInitialRegisterPool
  callerSavedRegisters :: [Register]
  callerSavedRegisters = rvCallerSavedRegisters
