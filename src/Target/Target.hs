{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Target.Target
  ( module Target.Target
  ) where

import Codegen.Common
import Ir

import Control.Monad.State
import Data.Map (Map)

import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import Optics
import Optics.State.Operators ((%=), (.=))

import qualified Data.Map as Map

data Arch
  = RV32
  | RV64

class RegisterAllocator target =>
      InstSelector target
  where
  initCodegen :: Int -> Int -> Map String FuncTypeSignature -> CodegenState target
  forceToReg :: VStackItem -> State (CodegenState target) Register
  forceToReg (Immediate (IntLiteral i)) = forceImmediateToReg i
  forceToReg (Immediate (CharLiteral _)) = error "Only Int literals are supported yet :)"
  forceToReg (Reg r) = pure r
  forceToReg (Spilled spillOffset) = do
    reg <- allocateRegister
    emit $ emitLoad reg (spRegister @target) spillOffset
    freeHwStackOffset spillOffset
    pure reg
  forceImmediateToReg :: Immediate -> State (CodegenState target) Register
  forceImmediateToReg i = do
    cachedReg <- use (#cache % at (Const i))
    case cachedReg of
      Just (Reg r) -> pure r
      _ -> do
        loadTo <- allocateRegister
        loadImmediate i loadTo
        pure loadTo
  handleStackStatesMerge :: [VStackItem] -> [VStackItem] -> State (CodegenState target) ()
  handleStackStatesMerge current target = do
    let toMerge = [(i, c, t) | (i, (c, t)) <- zip ([0 ..] :: [Int]) (zip current target), c /= t]
    case toMerge of
      [] -> return ()
      ((idx, curr, _):_) -> do
        let conflicts ct (_, c, _) =
              case ct of
                Reg r -> c == Reg r
                Spilled offset -> c == Spilled offset
                _ -> False
        let nonConflictingMoves = [(i, c, t) | (i, c, t) <- toMerge, not (any (conflicts t) (filter (\(j, _, _) -> j /= i) toMerge))]
        case nonConflictingMoves of
          ((i, c, t):_) -> do
            mergeItem c t
            handleStackStatesMerge (replaceVStackItem i t current) target
          [] -> do
            scratch <- allocateRegister
            mergeItem curr (Reg scratch)
            let currWithScratch = replaceVStackItem idx (Reg scratch) current
            handleStackStatesMerge currWithScratch target
            freeRegister scratch
    where
      mergeItem currentItem targetItem
        | currentItem == targetItem = return ()
      mergeItem currentItem targetItem =
        case (currentItem, targetItem) of
          (Immediate (IntLiteral currVal), Reg targetReg) -> loadImmediate currVal targetReg
          (Spilled hwOffset, Reg targetReg) -> emit $ emitLoad targetReg (spRegister @target) hwOffset
          (Reg currentReg, Reg targetReg) -> emitMove targetReg currentReg
          (Immediate (IntLiteral i), Spilled hwOffsetTarget) -> do
            tmp <- forceImmediateToReg i
            emit $ emitStore tmp (spRegister @target) hwOffsetTarget
            freeRegister tmp
          (Spilled hwOffsetCurrent, Spilled hwOffsetTarget) -> do
            tmp <- allocateRegister
            emit $ emitLoad tmp (spRegister @target) hwOffsetCurrent
            emit $ emitStore tmp (raRegister @target) hwOffsetTarget
            freeRegister tmp
          (Reg currentReg, Spilled hwOffsetTarget) -> emit $ emitStore currentReg (spRegister @target) hwOffsetTarget
          _ ->
            error
              $ "Unable to merge stack items on block change, current item: "
                  ++ show current
                  ++ " item requested by target: "
                  ++ show target
      replaceVStackItem _ _ [] = []
      replaceVStackItem 0 newVal (_:ts) = newVal : ts
      replaceVStackItem offset newVal (h:ts) = h : replaceVStackItem (offset - 1) newVal ts
  codegenAdd :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenSub :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenMul :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenDiv :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenMod :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenLt :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenLte :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenGt :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenGte :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenEq :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenNot :: VStackItem -> State (CodegenState target) VStackItem
  codegenBranchIfZero :: VStackItem -> String -> State (CodegenState target) ()
  codegenGetLocalAddr :: Int -> State (CodegenState target) VStackItem
  codegenLoad :: IrType -> Int -> VStackItem -> State (CodegenState target) VStackItem
  codegenStore :: IrType -> Int -> VStackItem -> VStackItem -> State (CodegenState target) ()
  codegenSyscall :: Int -> State (CodegenState target) ()
  codeGenPrintInt :: VStackItem -> State (CodegenState target) ()
  emitLoad :: Register -> Register -> Immediate -> target
  emitStore :: Register -> Register -> Immediate -> target
  emitAddi :: Register -> Register -> Immediate -> target
  emitMove :: Register -> Register -> State (CodegenState target) ()
  emitCall :: String -> IrType -> State (CodegenState target) [VStackItem]
  emitLabel :: String -> target
  emitJump :: String -> target
  emitBranchIfEqual :: Register -> Register -> String -> target
  emitFuncProlog :: FunctionDef -> Int -> [target]
  emitFuncEpilog :: Int -> String -> [target]
  loadImmediate :: Immediate -> Register -> State (CodegenState target) ()
  handleRegArgs :: (Int, VStackItem) -> State (CodegenState target) ()
  handleRegArgs (stackOffset, vStackItem) = do
    let argReg = (Register ("a" ++ show stackOffset) GeneralPurpose)
    case vStackItem of
      Immediate (IntLiteral i) -> loadImmediate i argReg
      Reg r -> do
        emitMove argReg r
        freeRegister r
      Spilled offset -> do
        emit $ emitLoad argReg (spRegister @target) offset
        freeHwStackOffset offset
      _ -> error "Unsupported type - only int Literals supported right now"
  handleMemArgs :: [VStackItem] -> State (CodegenState target) ()
  handleMemArgs memArgs =
    let memArgCount = length memArgs
        regSize = registerSize @target
     in when (memArgCount > 0) $ do
          let argsBytes = alignTo (spAlignment @target) (memArgCount * regSize)
          forM_ (zip ([0 ..] :: [Int]) memArgs) $ \(i, arg) -> pushToPhysStack (-argsBytes + (i * regSize)) arg
          emit $ bumpSp $ -argsBytes
  pushToPhysStack :: Int -> VStackItem -> State (CodegenState target) ()
  pushToPhysStack hwStackOffset toPush = do
    regToPush <- forceToReg toPush
    emit $ emitStore regToPush (spRegister @target) hwStackOffset
    freeRegister regToPush
  restoreSp :: Int -> State (CodegenState target) ()
  restoreSp memArgsCount = when (memArgsCount > 0) $ emit $ bumpSp $ memArgsCount * registerSize @target
  bumpSp :: Int -> target
  bumpSp bytes =
    let alignedBytes = alignTo (spAlignment @target) (abs bytes)
     in let bumpBy =
              if bytes >= 0
                then alignedBytes
                else -alignedBytes
         in emitAddi (spRegister @target) (spRegister @target) bumpBy
  spRegister :: Register
  raRegister :: Register
  returnValueRegisters :: [Register]
  spAlignment :: Int
  extraFrameSlotsCount :: Int
  registerSize :: Int
  funcArgumentsRegistersCount :: Int

class RegisterAllocator target where
  allocateRegister :: InstSelector target => State (CodegenState target) Register
  allocateRegister = do
    freeRegs <- use #freeRegisters
    case freeRegs of
      (allocated:rest) -> do
        #freeRegisters .= rest
        pure allocated
      [] -> do
        freedReg <- tryToFreeInactiveRegister
        case freedReg of
          Just r -> pure r
          Nothing -> handleRegistersSpill
  handleRegistersSpill :: InstSelector target => State (CodegenState target) Register
  handleRegistersSpill = do
    activeRegisters <- getActiveRegisters
    when (length activeRegisters == 0) $ error "Error: run out of CPU register and there are also non to be spilled to memory"
    let toSpill = last activeRegisters
    spillOffset <- allocateHwStackOffset
    invalidateCacheLine $ Reg toSpill
    #virtualStack %= map (spillRegisterHelper toSpill spillOffset)
    emit $ emitStore toSpill (spRegister @target) spillOffset
    pure toSpill
    where
      spillRegisterHelper toSpill offset =
        \item ->
          if item == (Reg toSpill)
            then Spilled offset
            else item
  tryToFreeInactiveRegister :: State (CodegenState target) (Maybe Register)
  tryToFreeInactiveRegister = do
    inCacheOnly <- getCachedInactiveRegisters
    case inCacheOnly of
      (toFree:_) -> do
        invalidateCacheLine $ Reg toFree
        pure $ Just toFree
      [] -> pure $ Nothing
  allocateHwStackOffset :: InstSelector target => State (CodegenState target) HwStackOffset
  allocateHwStackOffset = do
    freeOffsets <- use #freeSpillOffsets
    case freeOffsets of
      (allocated:rest) -> do
        #freeSpillOffsets .= rest
        pure allocated
      [] -> do
        offset <- use #nextSpillOffset
        #nextSpillOffset %= (+ (registerSize @target))
        pure offset
  freeHwStackOffset :: HwStackOffset -> State (CodegenState target) ()
  freeHwStackOffset offset = do
    vStack <- use #virtualStack
    let isReferenced =
          any
            (\case
               Spilled o -> o == offset
               _ -> False)
            vStack
    when (not isReferenced) $ #freeSpillOffsets %= (offset :)
  getActiveRegisters :: State (CodegenState target) [Register]
  getActiveRegisters = do
    vStack <- use #virtualStack
    pure [r | Reg r <- vStack]
  getCachedInactiveRegisters :: State (CodegenState target) [Register]
  getCachedInactiveRegisters = do
    cached <- use #cache
    activeRegisters <- getActiveRegisters
    pure $ [r | (_, Reg r) <- Map.toList cached, r `notElem` activeRegisters]
  initialRegisterPool :: [Register]
  callerSavedRegisters :: [Register]

emit :: inst -> State (CodegenState inst) ()
emit inst = #emittedCode %= (inst :)

alignTo :: Int -> Int -> Int
alignTo alignment x = (x + (alignment - 1)) .&. (-alignment)

is12BitsImm :: Immediate -> Bool
is12BitsImm i = i >= -2048 && i <= 2047
