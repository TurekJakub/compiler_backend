{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}



module Target.Riscv.Common
  ( module Target.Riscv.Common
  ) where

import Control.Monad.State ( State )
import GHC.Generics (Generic)
import Ir (LabelName, Literal(IntLiteral), FuncTypeSignature)
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
import Ir
import Codegen.Common

import Control.Monad.State
import Data.Map (Map)

import Optics
import Optics.State.Operators ((.=))

import qualified Data.Map as Map
import Target.Target
  ( InstSelector(..)
  , RegisterAllocator(initialRegisterPool)
  , emit
  , is12BitsImm
  )


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
     InstSelector (RiscVInst (a::Arch))
  => VStackItem
  -> VStackItem
  -> (BinOpDefinition a)
  -> State (CodegenState (RiscVInst (a::Arch))) VStackItem
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
    (Spilled offset, Immediate (IntLiteral i1))
      | Just handler <- binOpDef ^. #immToRegCodegen -> do
        r1 <- forceToReg $ Spilled offset
        handleImmediate handler i1 r1
    (Immediate (IntLiteral i1), Spilled offset)
      | Just handler <- binOpDef ^. #regToImmCodegen -> do
        r1 <- forceToReg $ Spilled offset
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

rvInitCodegen :: forall target allocator . (InstSelector target, RegisterAllocator allocator) =>  Int -> Int -> Map String FuncTypeSignature -> CodegenState target
rvInitCodegen argsCount frameSize knowFuncDefs =
    let initialCache =
          Map.fromList
            [ if i < 8
              then ( Var ("arg" ++ show i)
                   , Reg (Register ("a" ++ show i) GeneralPurpose))
              else ( Var ("arg" ++ show i)
                   , Spilled (frameSize + (i - 8) * registerSize @target))
            | i <- [0 .. argsCount - 1]
            ]
     in CodegenState
          { virtualStack = [] -- Do not push arguments to stack right away, they will be lazy-loaded from cache on demand   
          , freeRegisters = (initialRegisterPool @allocator)
          , cache = initialCache
          , emittedCode = []
          , knowFuncDef = knowFuncDefs
          , localVars = Map.empty
          , freeSpillOffsets = []
          , nextSpillOffset = 0
          , blockStackStates = Map.empty
          }
rvCodegenAdd :: forall (a :: Arch). InstSelector (RiscVInst a) =>  VStackItem -> VStackItem -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenAdd lhs rhs =
  let addDef =
        BinOpDefinition
          { regToRegCodegen = \t r1 r2 -> emit $ RV_Add t r1 r2
          , regToImmCodegen = Just $ addiCodegen
          , immToRegCodegen = Just $ addiCodegen
          }
   in binCodgenOpHelper lhs rhs addDef
  where
    addiCodegen tar reg imm = emit $ RV_Addi tar reg imm

rvCodegenSub ::  forall (a :: Arch). InstSelector (RiscVInst a) => VStackItem -> VStackItem -> State (CodegenState  (RiscVInst a)) VStackItem
rvCodegenSub lhs rhs =
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
rvCodegenMul :: forall (a :: Arch). InstSelector (RiscVInst a) => VStackItem -> VStackItem -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenMul lhs rhs =
  let mulDef =
        BinOpDefinition
          { regToRegCodegen = \t r1 r2 -> emit $ Rv_Mul t r1 r2
          , regToImmCodegen = Nothing
          , immToRegCodegen = Nothing
          }
   in binCodgenOpHelper lhs rhs mulDef

rvCodegenBranchIfZero :: forall (a :: Arch). InstSelector (RiscVInst a) => VStackItem -> String -> State (CodegenState (RiscVInst a)) ()
rvCodegenBranchIfZero precedent target =
  case precedent of
    Reg pr -> do
      emit $ RV_Beq pr rvZeroRegister target
      freeRegister pr
    Spilled offset -> do
      tmp <- forceToReg $ Spilled offset
      emit $ RV_Beq tmp rvZeroRegister target
      freeRegister tmp
    _ -> return () -- This should be handled by generic codegen pass
rvLoadImmediate :: forall (a :: Arch). InstSelector (RiscVInst a) => Immediate -> Register -> State (CodegenState (RiscVInst a)) ()
rvLoadImmediate imm reg = do
  #cache % at (Const imm) .= Just (Reg reg)
  emit $ RV_Li reg imm
  {-
  emitLoad :: Register -> Register -> Immediate -> Rv64Inst
  emitLoad target src offset = RV_Ld target src offset
  emitStore :: Register -> Register -> Immediate -> Rv64Inst
  emitStore src targetAddr offset = RV_Sd src targetAddr offset
  -}
rvEmitAddi :: forall (a :: Arch). InstSelector (RiscVInst a) => Register -> Register -> Immediate -> (RiscVInst a)
rvEmitAddi target r i = RV_Addi target r i
rvEmitMove :: forall (a :: Arch). InstSelector (RiscVInst a) => Register -> Register -> State (CodegenState (RiscVInst a)) ()
rvEmitMove r1 r2 =
  if r1 /= r2
    then emit $ Rv_Mv r1 r2
    else return ()
rvEmitCall ::  forall (a :: Arch). (InstSelector (RiscVInst a), RegisterAllocator(RiscVInst a) ) => String -> IrType -> State (CodegenState (RiscVInst a)) [VStackItem]
rvEmitCall callee retType = do
  invalidateCache
  emit $ RV_Call callee
  case retType of
    VoidType -> return []
    IntType -> do
      case (returnValueRegisters @(RiscVInst a)) of
        (retReg:_) -> return [Reg $ retReg]
        _ ->
          error
            "There are no return value registers defined in target definition" -- This should never happened, implies backend target author error
    _ -> error "Only void and int return types are supported right now"
rvEmitLabel :: String -> (RiscVInst a)
rvEmitLabel labelName = RV_Label labelName
rvEmitJump :: String -> (RiscVInst a)
rvEmitJump target = RV_J target
rvEmitBranchIfEqual :: Register -> Register -> String -> (RiscVInst a)
rvEmitBranchIfEqual lhs rhs target = RV_Beq lhs rhs target
rvEmitFuncProlog ::forall (a :: Arch). (InstSelector (RiscVInst a), RegisterAllocator (RiscVInst a)) => FunctionDef -> Int -> [(RiscVInst a)]
rvEmitFuncProlog funcDef funcFrameSize =
  let raOffset = getRaOffset funcFrameSize (registerSize @(RiscVInst a))
   in [ RV_Label $ view (#prototype % #name) funcDef
      , bumpSp @(RiscVInst a) (-funcFrameSize)
      , emitStore (raRegister @(RiscVInst a)) (spRegister @(RiscVInst a)) raOffset  --RV_Sd (raRegister @(RiscVInst a)) (spRegister @(RiscVInst a)) raOffset
      ]
rvEmitFuncEpilog :: forall (a :: Arch). (InstSelector (RiscVInst a), RegisterAllocator (RiscVInst a)) => Int -> [(RiscVInst a)]
rvEmitFuncEpilog funcFrameSize =
  let raOffset = getRaOffset funcFrameSize (registerSize @(RiscVInst a))
   in [ emitLoad (raRegister @(RiscVInst a)) (spRegister @(RiscVInst a)) raOffset
      , bumpSp funcFrameSize
      , RV_Ret
      ]
rvSpRegister :: Register
rvSpRegister = Register "sp" GeneralPurpose
rvRaRegister :: Register
rvRaRegister = Register "ra" GeneralPurpose
rvReturnValueRegisters :: [Register]
rvReturnValueRegisters = [Register "a0" GeneralPurpose]
rvSpAlignment :: Int
rvSpAlignment = 16
rvExtraFrameSlotsCount :: Int
rvExtraFrameSlotsCount = 1
rvFuncArgumentsRegistersCount :: Int
rvFuncArgumentsRegistersCount = 8

rvInitialRegisterPool :: [Register]
rvInitialRegisterPool =
    map (\n -> Register ("t" ++ show n) GeneralPurpose) ([0 .. 6] :: [Int])



getRaOffset :: Int -> Int -> Int
getRaOffset funcFrameSize regSize= funcFrameSize - regSize