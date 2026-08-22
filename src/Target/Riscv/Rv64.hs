{-# OPTIONS_GHC -Wno-orphans #-}

module Target.Riscv.Rv64
  () where

import Ir
import Codegen.Common

import Control.Monad.State
import Data.Map (Map)

import Target.Riscv.Common
import Target.Target
  ( InstSelector(..)
  , RegisterAllocator(initialRegisterPool)
 
  )

instance InstSelector Rv64Inst where
  initCodegen :: Int -> Int -> Map String FuncTypeSignature -> CodegenState Rv64Inst
  initCodegen = rvInitCodegen @Rv64Inst @Rv64Inst

  codegenAdd :: VStackItem-> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenAdd = rvCodegenAdd

  codegenSub :: VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenSub = rvCodegenSub

  codegenMul :: VStackItem-> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenMul = rvCodegenMul

  codegenBranchIfZero :: VStackItem -> String -> State (CodegenState Rv64Inst) ()
  codegenBranchIfZero = rvCodegenBranchIfZero

  loadImmediate :: Immediate -> Register -> State (CodegenState Rv64Inst) ()
  loadImmediate = rvLoadImmediate
  emitLoad :: Register -> Register -> Immediate -> Rv64Inst
  emitLoad target src offset = RV_Ld target src offset
  emitStore :: Register -> Register -> Immediate -> Rv64Inst
  emitStore src targetAddr offset = RV_Sd src targetAddr offset
  emitAddi :: Register -> Register -> Immediate -> Rv64Inst
  emitAddi = rvEmitAddi
  emitMove :: Register -> Register -> State (CodegenState Rv64Inst) ()
  emitMove = rvEmitMove
  emitCall :: String -> IrType -> State (CodegenState Rv64Inst) [VStackItem]
  emitCall =rvEmitCall
  emitLabel :: String -> Rv64Inst
  emitLabel =rvEmitLabel
  emitJump :: String -> Rv64Inst
  emitJump =rvEmitJump
  emitBranchIfEqual :: Register -> Register -> String -> Rv64Inst
  emitBranchIfEqual = rvEmitBranchIfEqual
  emitFuncProlog :: FunctionDef -> Int -> [Rv64Inst]
  emitFuncProlog = rvEmitFuncProlog
  emitFuncEpilog :: Int -> [Rv64Inst]
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
  registerSize = 8

instance RegisterAllocator Rv64Inst where
  initialRegisterPool :: [Register]
  initialRegisterPool =rvInitialRegisterPool