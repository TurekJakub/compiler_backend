{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Target.Riscv.Rv64
  ( module Target.Riscv.Rv64
  ) where

import Ir
import Codegen.Common

import Control.Monad.State
import Data.Map (Map)

import Optics
import Optics.State.Operators ((.=))

import qualified Data.Map as Map
import Target.Riscv.Common
import Target.Target
  ( InstSelector(..)
  , RegisterAllocator(initialRegisterPool)
  , emit
  , is12BitsImm
  )

instance InstSelector Rv64Inst where
  initCodegen ::
       Int -> Int -> Map String FuncTypeSignature -> CodegenState Rv64Inst
  initCodegen argsCount frameSize knowFuncDefs =
    let initialCache =
          Map.fromList
            [ if i < 8
              then ( Var ("arg" ++ show i)
                   , Reg (Register ("a" ++ show i) GeneralPurpose))
              else ( Var ("arg" ++ show i)
                   , Spilled (frameSize + (i - 8) * registerSize @Rv64Inst))
            | i <- [0 .. argsCount - 1]
            ]
     in CodegenState
          { virtualStack = [] -- Do not push arguments to stack right away, they will be lazy-loaded from cache on demand   
          , freeRegisters = (initialRegisterPool @Rv64Inst)
          , cache = initialCache
          , emittedCode = []
          , knowFuncDef = knowFuncDefs
          , localVars = Map.empty
          , freeSpillOffsets = []
          , nextSpillOffset = 0
          , blockStackStates = Map.empty
          }
  codegenAdd ::
       VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenAdd lhs rhs =
    let addDef =
          BinOpDefinition
            { regToRegCodegen = \t r1 r2 -> emit $ RV_Add t r1 r2
            , regToImmCodegen = Just $ addiCodegen
            , immToRegCodegen = Just $ addiCodegen
            }
     in binCodgenOpHelper lhs rhs addDef
    where
      addiCodegen tar reg imm = emit $ RV_Addi tar reg imm
  codegenSub ::
       VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenSub lhs rhs =
    let subDef =
          BinOpDefinition
            { regToRegCodegen = \t r1 r2 -> emit $ RV_Sub t r1 r2
            , regToImmCodegen =
                Just $ \t r1 i1 -> do
                  emit $ RV_Sub t rvZeroRegister r1
                  emit $ RV_Addi t t i1
            , immToRegCodegen =
                Just $ \t r1 i1 ->
                  if is12BitsImm $ -i1
                    then emit $ RV_Addi t r1 (-i1)
                    else do
                      tmp <- forceToReg $ Immediate $ IntLiteral i1
                      emit $ RV_Sub t r1 tmp
                      freeRegister tmp
            }
     in binCodgenOpHelper lhs rhs subDef
  codegenMul ::
       VStackItem -> VStackItem -> State (CodegenState Rv64Inst) VStackItem
  codegenMul lhs rhs =
    let mulDef =
          BinOpDefinition
            { regToRegCodegen = \t r1 r2 -> emit $ Rv_Mul t r1 r2
            , regToImmCodegen = Nothing
            , immToRegCodegen = Nothing
            }
     in binCodgenOpHelper lhs rhs mulDef
  codegenBranchIfZero ::
       VStackItem -> String -> State (CodegenState Rv64Inst) ()
  codegenBranchIfZero precedent target =
    case precedent of
      Reg pr -> do
        emit $ RV_Beq pr rvZeroRegister target
        freeRegister pr
      Spilled offset -> do
        tmp <- forceToReg $ Spilled offset
        emit $ RV_Beq tmp rvZeroRegister target
        freeRegister tmp
      _ -> return () -- This should be handled by generic codegen pass
  loadImmediate :: Immediate -> Register -> State (CodegenState Rv64Inst) ()
  loadImmediate imm reg = do
    #cache % at (Const imm) .= Just (Reg reg)
    emit $ RV_Li reg imm
  emitLoad :: Register -> Register -> Immediate -> Rv64Inst
  emitLoad target src offset = RV_Ld target src offset
  emitStore :: Register -> Register -> Immediate -> Rv64Inst
  emitStore src targetAddr offset = RV_Sd src targetAddr offset
  emitAddi :: Register -> Register -> Immediate -> Rv64Inst
  emitAddi target r i = RV_Addi target r i
  emitMove :: Register -> Register -> State (CodegenState Rv64Inst) ()
  emitMove r1 r2 =
    if r1 /= r2
      then emit $ Rv_Mv r1 r2
      else return ()
  emitCall :: String -> IrType -> State (CodegenState Rv64Inst) [VStackItem]
  emitCall callee retType = do
    invalidateCache
    emit $ RV_Call callee
    case retType of
      VoidType -> return []
      IntType -> do
        case (returnValueRegisters @Rv64Inst) of
          (retReg:_) -> return [Reg $ retReg]
          _ ->
            error
              "There are no return value registers defined in target definition" -- This should never happened, implies backend target author error
      _ -> error "Only void and int return types are supported right now"
  emitLabel :: String -> Rv64Inst
  emitLabel labelName = RV_Label labelName
  emitJump :: String -> Rv64Inst
  emitJump target = RV_J target
  emitBranchIfEqual :: Register -> Register -> String -> Rv64Inst
  emitBranchIfEqual lhs rhs target = RV_Beq lhs rhs target
  emitFuncProlog :: FunctionDef -> Int -> [Rv64Inst]
  emitFuncProlog funcDef funcFrameSize =
    let raOffset = getRaOffset funcFrameSize
     in [ RV_Label $ view (#prototype % #name) funcDef
        , bumpSp @Rv64Inst (-funcFrameSize)
        , RV_Sd (raRegister @Rv64Inst) (spRegister @Rv64Inst) raOffset
        ]
  emitFuncEpilog :: Int -> [Rv64Inst]
  emitFuncEpilog funcFrameSize =
    let raOffset = getRaOffset funcFrameSize
     in [ RV_Ld (raRegister @Rv64Inst) (spRegister @Rv64Inst) raOffset
        , bumpSp funcFrameSize
        , RV_Ret
        ]
  spRegister :: Register
  spRegister = Register "sp" GeneralPurpose
  raRegister :: Register
  raRegister = Register "ra" GeneralPurpose
  returnValueRegisters :: [Register]
  returnValueRegisters = [Register "a0" GeneralPurpose]
  spAlignment :: Int
  spAlignment = 16
  extraFrameSlotsCount :: Int
  extraFrameSlotsCount = 1
  registerSize :: Int
  registerSize = 8
  funcArgumentsRegistersCount :: Int
  funcArgumentsRegistersCount = 8

getRaOffset :: Int -> Int
getRaOffset funcFrameSize = funcFrameSize - registerSize @Rv64Inst

instance RegisterAllocator Rv64Inst where
  initialRegisterPool :: [Register]
  initialRegisterPool =
    map (\n -> Register ("t" ++ show n) GeneralPurpose) ([0 .. 6] :: [Int])
