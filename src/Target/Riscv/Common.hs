{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Target.Riscv.Common
  ( module Target.Riscv.Common
  ) where

import GHC.Generics (Generic)

import Codegen.Common
import Ir
import Target.Target (Arch(..), is12BitsImm)

import Control.Monad.State
import Data.Map (Map)
import Optics
import Optics.State.Operators ((.=))

import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Target.Target
  ( InstSelector(..)
  , RegisterAllocator(allocateRegister, initialRegisterPool)
  , emit
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
  Rv_Div :: Register -> Register -> Register -> RiscVInst a
  Rv_Rem :: Register -> Register -> Register -> RiscVInst a
  RV_Slt :: Register -> Register -> Register -> RiscVInst a
  RV_Slti :: Register -> Register -> Immediate -> RiscVInst a
  RV_Sltiu :: Register -> Register -> Immediate -> RiscVInst a
  RV_And :: Register -> Register -> Register -> RiscVInst a
  RV_Andi :: Register -> Register -> Immediate -> RiscVInst a
  RV_Or :: Register -> Register -> Register -> RiscVInst a
  RV_Ori :: Register -> Register -> Immediate -> RiscVInst a
  RV_Xor :: Register -> Register -> Register -> RiscVInst a
  RV_Xori :: Register -> Register -> Immediate -> RiscVInst a
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

type ImmRegBinOpCodegen (a :: Arch)
  = Register -> Register -> Immediate -> State (CodegenState (RiscVInst a)) ()

data BinOpDefinition (a :: Arch) = BinOpDefinition
  { regToRegCodegen :: Register -> Register -> Register -> State
                                                             (CodegenState
                                                                (RiscVInst a))
                                                             ()
  , immToRegCodegen :: Maybe (ImmRegBinOpCodegen a)
  , regToImmCodegen :: Maybe (ImmRegBinOpCodegen a)
  } deriving (Generic)

binCodgenOpHelper ::
     InstSelector (RiscVInst (a :: Arch))
  => VStackItem
  -> VStackItem
  -> (BinOpDefinition a)
  -> State (CodegenState (RiscVInst (a :: Arch))) VStackItem
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
      target <- allocateRegister
      (binOpDef ^. #regToRegCodegen) target lhsTmp rhsTmp
      {-
      
      freeRegister lhsTmp
      let res = Reg rhsTmp
      invalidateCacheLine res
      -}
      pure $ Reg target
    handleImmediate codgen i1 r1 =
      if is12BitsImm i1
        then do
          target <- allocateRegister
          codgen target r1 i1
         -- let res = Reg r1
         -- invalidateCacheLine res
          pure $ Reg target
        else genericCodgen

notBinOpDef :: BinOpDefinition a -> BinOpDefinition a
notBinOpDef def =
  BinOpDefinition
    { regToRegCodegen = \t r1 r2 -> regToRegCodegen def t r1 r2 >> (emit $ rvEmitLogicalNot t t)
    , immToRegCodegen = (\f t r imm -> f t r imm >> (emit $ rvEmitLogicalNot t t)) <$> def ^. #immToRegCodegen
    , regToImmCodegen = (\f t r imm -> f t r imm >> (emit $ rvEmitLogicalNot t t)) <$> def ^. #regToImmCodegen
    }

flipBinOpDef :: BinOpDefinition a -> BinOpDefinition a
flipBinOpDef def =
  BinOpDefinition
    { regToRegCodegen = \t r1 r2 -> regToRegCodegen def t r2 r1
    , immToRegCodegen = regToImmCodegen def
    , regToImmCodegen = immToRegCodegen def
    }

foldAddressHelper ::
     forall (a :: Arch).
     (InstSelector (RiscVInst a), RegisterAllocator (RiscVInst a))
  => VStackItem
  -> Int
  -> State (CodegenState (RiscVInst a)) (Register, Int, Bool)
foldAddressHelper addr offset =
  case addr of
    (Immediate (IntLiteral addrImm)) -> do
      let newOffset = offset + addrImm
      if is12BitsImm newOffset
        then return (rvZeroRegister, newOffset, False)
        else do
          foldedAddrReg <- forceImmediateToReg newOffset
          return (foldedAddrReg, 0, True)
    (Immediate _) -> error ""
    _ -> do
      addrReg <- forceToReg addr
      if is12BitsImm offset
        then return (addrReg, offset, False)
        else do
          offsetReg <- forceImmediateToReg offset
          foldedAddrReg <- allocateRegister
          emit $ RV_Add foldedAddrReg addrReg offsetReg
          freeRegister offsetReg
          return (foldedAddrReg, 0, True)

rvInitCodegen ::
     forall target allocator. (InstSelector target, RegisterAllocator allocator)
  => Int
  -> Int
  -> Map String FuncTypeSignature
  -> CodegenState target
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
        { virtualStack = [] -- Do not push arguments to stack right away, they will be lazy-loaded from cache on demand - This is bad and needs a rework  
        , freeRegisters = (initialRegisterPool @allocator)
        , cache = initialCache
        , emittedCode = []
        , knowFuncDef = knowFuncDefs
        , localVars = Map.empty -- args need to be handled as full blown local. With current setup they get lost on first cache wipe, for example on any jump/control flow change
        , freeSpillOffsets = []
        , nextSpillOffset = 0
        , blockStackStates = Map.empty
        , notCachedLocals = Set.empty
        }

rvCodegenAdd ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
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

rvCodegenSub ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenSub lhs rhs =
  let subDef =
        BinOpDefinition
          { regToRegCodegen = \t r1 r2 -> emit $ RV_Sub t r1 r2
          , immToRegCodegen =
              Just $ \t r1 i1 -> do
                emit $ RV_Sub t rvZeroRegister r1
                emit $ RV_Addi t t i1
          , regToImmCodegen =
              Just $ \t r1 i1 ->
                if is12BitsImm $ -i1
                  then emit $ RV_Addi t r1 (-i1)
                  else do
                    tmp <- forceToReg $ Immediate $ IntLiteral i1
                    emit $ RV_Sub t r1 tmp
                    freeRegister tmp
          }
   in binCodgenOpHelper lhs rhs subDef

rvCodegenMul ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenMul lhs rhs =
  let mulDef = BinOpDefinition {regToRegCodegen = \t r1 r2 -> emit $ Rv_Mul t r1 r2, regToImmCodegen = Nothing, immToRegCodegen = Nothing}
   in binCodgenOpHelper lhs rhs mulDef

rvCodegenDiv ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenDiv lhs rhs =
  let divDef = BinOpDefinition {regToRegCodegen = \t r1 r2 -> emit $ Rv_Div t r1 r2, regToImmCodegen = Nothing, immToRegCodegen = Nothing}
   in binCodgenOpHelper lhs rhs divDef

rvCodegenMod ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenMod lhs rhs =
  let modDef = BinOpDefinition {regToRegCodegen = \t r1 r2 -> emit $ Rv_Rem t r1 r2, regToImmCodegen = Nothing, immToRegCodegen = Nothing}
   in binCodgenOpHelper lhs rhs modDef

rvLtDef :: BinOpDefinition a
rvLtDef =
  BinOpDefinition
    { regToRegCodegen = \t r1 r2 -> emit $ RV_Slt t r1 r2
    , regToImmCodegen =
        Just $ \t r1 imm -> do
          emit $ RV_Slti t r1 (imm + 1)
          emit $ RV_Xori t t 1
    , immToRegCodegen = Just $ \t r1 imm -> emit $ RV_Slti t r1 imm
    }

rvCodegenLt ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenLt lhs rhs = binCodgenOpHelper lhs rhs rvLtDef

rvCodegenLte ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenLte lhs rhs = binCodgenOpHelper lhs rhs (notBinOpDef $ flipBinOpDef rvLtDef)

rvCodegenGt ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenGt lhs rhs =
  let gtDef = BinOpDefinition {regToRegCodegen = \t r1 r2 -> emit $ RV_Slt t r1 r2, immToRegCodegen = Nothing, regToImmCodegen = Nothing}
   in binCodgenOpHelper lhs rhs gtDef

rvCodegenGte ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenGte lhs rhs = binCodgenOpHelper lhs rhs (notBinOpDef rvLtDef)

rvCodegenEq ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenEq lhs rhs =
  let eqDef =
        BinOpDefinition
          { regToRegCodegen =
              \t r1 r2 -> do
                emit $ RV_Xor t r1 r2
                emit $ RV_Sltiu t t 1
          , immToRegCodegen =
              Just $ \t r1 imm -> do
                emit $ RV_Xori t r1 imm
                emit $ RV_Sltiu t t 1
          , regToImmCodegen =
              Just $ \t r1 imm -> do
                emit $ RV_Xori t r1 imm
                emit $ RV_Sltiu t t 1
          }
   in binCodgenOpHelper lhs rhs eqDef

rvCodegenLogicalNot ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenLogicalNot operand =
  case operand of
    (Reg r) -> do
      emit $ rvEmitLogicalNot r r
      pure $ Reg r
    (Spilled _) -> do
      reg <- forceToReg operand
      emit $ rvEmitLogicalNot reg reg
      pure $ Reg reg
    (Immediate _) ->
      error "Constant folding should be handled by general codegen pass"

rvCodegenBranchIfZero ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => VStackItem
  -> String
  -> State (CodegenState (RiscVInst a)) ()
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

rvCodegenGetLocalAddr ::
     forall (a :: Arch).
     (InstSelector (RiscVInst a), RegisterAllocator (RiscVInst a))
  => Int
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenGetLocalAddr offset =
  if is12BitsImm offset
    then do
      addrReg <- allocateRegister @(RiscVInst a)
      emit $ RV_Addi addrReg (spRegister @(RiscVInst a)) offset
      return $ Reg addrReg
    else do
      offsetReg <- forceImmediateToReg offset
      addrReg <- allocateRegister
      emit $ RV_Add addrReg (spRegister @(RiscVInst a)) offsetReg
      freeRegister offsetReg
      return $ Reg addrReg

rvCodegenLoad ::
     forall (a :: Arch).
     (InstSelector (RiscVInst a), RegisterAllocator (RiscVInst a))
  => IrType
  -> Int
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) VStackItem
rvCodegenLoad dataType offset addr = do
  (addReg, foldedOffset, isAddrTmp) <- foldAddressHelper addr offset
  case dataType of
    IntType -> do
      loaded <- allocateRegister @(RiscVInst a)
      emit $ emitLoad loaded addReg foldedOffset -- integers are of target architecture reg size => emit lw/ld for rv32/rv64
      when isAddrTmp $ freeRegister @(RiscVInst a) addReg
      return $ Reg loaded
    _ -> error "Load of non integer values not implemented yet"

rvCodegenStore ::
     forall (a :: Arch).
     (InstSelector (RiscVInst a), RegisterAllocator (RiscVInst a))
  => IrType
  -> Int
  -> VStackItem
  -> VStackItem
  -> State (CodegenState (RiscVInst a)) ()
rvCodegenStore dataType offset addr toStore = do
  (addReg, foldedOffset, isAddrTmp) <- foldAddressHelper addr offset
  toStoreReg <- forceToReg toStore
  case dataType of
    IntType -> do
      emit $ emitStore toStoreReg addReg foldedOffset
      when isAddrTmp $ freeRegister addReg
    _ -> error "Store of non integer values not implemented yet"

rvLoadImmediate ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => Immediate
  -> Register
  -> State (CodegenState (RiscVInst a)) ()
rvLoadImmediate imm reg = do
  #cache % at (Const imm) .= Just (Reg reg)
  emit $ RV_Li reg imm

rvEmitAddi ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => Register
  -> Register
  -> Immediate
  -> (RiscVInst a)
rvEmitAddi target r i = RV_Addi target r i

rvEmitMove ::
     forall (a :: Arch). InstSelector (RiscVInst a)
  => Register
  -> Register
  -> State (CodegenState (RiscVInst a)) ()
rvEmitMove r1 r2 =
  if r1 /= r2
    then emit $ Rv_Mv r1 r2
    else return ()

rvEmitCall ::
     forall (a :: Arch).
     (InstSelector (RiscVInst a), RegisterAllocator (RiscVInst a))
  => String
  -> IrType
  -> State (CodegenState (RiscVInst a)) [VStackItem]
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

rvEmitLogicalNot :: Register -> Register -> RiscVInst a
rvEmitLogicalNot r1 r2 = RV_Xori r1 r2 1

rvEmitFuncProlog ::
     forall (a :: Arch).
     (InstSelector (RiscVInst a), RegisterAllocator (RiscVInst a))
  => FunctionDef
  -> Int
  -> [(RiscVInst a)]
rvEmitFuncProlog funcDef funcFrameSize =
  let raOffset = getRaOffset funcFrameSize (registerSize @(RiscVInst a))
   in [ RV_Label $ view (#prototype % #name) funcDef
      , bumpSp @(RiscVInst a) (-funcFrameSize)
      , emitStore
          (raRegister @(RiscVInst a))
          (spRegister @(RiscVInst a))
          raOffset --RV_Sd (raRegister @(RiscVInst a)) (spRegister @(RiscVInst a)) raOffset
      ]

rvEmitFuncEpilog ::
     forall (a :: Arch).
     (InstSelector (RiscVInst a), RegisterAllocator (RiscVInst a))
  => Int
  -> [(RiscVInst a)]
rvEmitFuncEpilog funcFrameSize =
  let raOffset = getRaOffset funcFrameSize (registerSize @(RiscVInst a))
   in [ emitLoad
          (raRegister @(RiscVInst a))
          (spRegister @(RiscVInst a))
          raOffset
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
rvExtraFrameSlotsCount = 16

rvFuncArgumentsRegistersCount :: Int
rvFuncArgumentsRegistersCount = 8

rvInitialRegisterPool :: [Register]
rvInitialRegisterPool =
  map (\n -> Register ("t" ++ show n) GeneralPurpose) ([0 .. 6] :: [Int])

getRaOffset :: Int -> Int -> Int
getRaOffset funcFrameSize regSize = funcFrameSize - regSize
