{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Target
  ( module Target
  ) where

import Control.Monad.State (State)
import Abi
import Ir

import Control.Monad.State
import Data.Map (Map)

import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import Data.Containers.ListUtils (nubOrd)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=))

import qualified Data.Map as Map
import Lib
import Data.Proxy (Proxy (..)) 



class RegisterAllocator target => InstSelector target where
  forceToReg :: VStackItem -> State (CodegenState target) Register
  forceToReg (Immediate (IntLiteral i)) = forceImmediateToReg i
  forceToReg (Immediate (CharLiteral _)) =
    error "Only Int literals are supported yet :)"
  forceToReg (Reg r) = pure r
  forceToReg (Spilled spillOffset) = do
    reg <- allocateRegister
    emit $ emitLoad reg (spRegister $ Proxy @target) spillOffset
    freeHwStackOffset spillOffset
    pure reg

  forceImmediateToReg ::  Immediate -> State (CodegenState target) Register
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
    let toMerge =
          [ (i, c, t)
          | (i, (c, t)) <- zip ([0 ..] :: [Int]) (zip current target)
          , c /= t
          ]
    case toMerge of
      [] -> return ()
      ((idx, curr, _):_) -> do
        let conflicts ct (_, c, _) =
              case ct of
                Reg r -> c == Reg r
                Spilled offset -> c == Spilled offset
                _ -> False
        let nonConflictingMoves =
              [ (i, c, t)
              | (i, c, t) <- toMerge
              , not (any (conflicts t) (filter (\(j, _, _) -> j /= i) toMerge))
              ]
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
          (Immediate (IntLiteral currVal), Reg targetReg) ->
            loadImmediate currVal targetReg
          (Spilled hwOffset, Reg targetReg) ->
            emit $ emitLoad targetReg rvSpRegister hwOffset
          (Reg currentReg, Reg targetReg) -> emitMove targetReg currentReg
          (Immediate (IntLiteral i), Spilled hwOffsetTarget) -> do
            tmp <- forceImmediateToReg i
            emit $ emitStore tmp rvSpRegister hwOffsetTarget
            freeRegister tmp
          (Spilled hwOffsetCurrent, Spilled hwOffsetTarget) -> do
            tmp <- allocateRegister
            emit $ emitLoad tmp rvSpRegister hwOffsetCurrent
            emit $ emitStore tmp rvRaRegister hwOffsetTarget
            freeRegister tmp
          (Reg currentReg, Spilled hwOffsetTarget) ->
            emit $ emitStore currentReg rvSpRegister hwOffsetTarget
          _ ->
            error
              $ "Unable to merge stack items on block change, current item: "
                  ++ show current
                  ++ " item requested by target: "
                  ++ show target
      replaceVStackItem _ _ [] = []
      replaceVStackItem 0 newVal (_:ts) = newVal : ts
      replaceVStackItem offset newVal (h:ts) =
        h : replaceVStackItem (offset - 1) newVal ts


  codegenAdd :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenSub :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenMul :: VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenFuncCall :: String -> State (CodegenState target) ()

  emitLoad :: Register -> Register -> Immediate -> target
  emitStore :: Register -> Register -> Immediate -> target
  emitAddi :: Register -> Register -> Immediate -> target
  emitMove :: Register -> Register -> State (CodegenState target) ()
  emitCall :: String -> State (CodegenState target) ()

  loadImmediate :: Immediate -> Register -> State (CodegenState target) ()

  bumpSp :: Int -> target
  bumpSp bytes =
    let alignedBytes = alignTo rvSpAlignment (abs bytes)
    in let bumpBy =
              if bytes >= 0
                then alignedBytes
                else -alignedBytes
         in emitAddi rvSpRegister rvSpRegister bumpBy

  spRegister :: Proxy target -> Register
  raRegister :: Proxy target -> Register
  spAlignment :: Proxy target -> Int
  extraFrameSlotsCount :: Proxy target -> Int
  registerSize :: Proxy target -> Int


instance InstSelector RiscVInst  where
  codegenAdd :: VStackItem -> VStackItem -> State (CodegenState RiscVInst) VStackItem
  codegenAdd lhs rhs = 
    case (lhs, rhs) of
    (Reg r1, Reg r2) -> do
        emit $ RV_Add r2 r1 r2
        pure $ Reg r2
    ((Immediate (IntLiteral i)), (Reg r)) ->
        addiEmitter r i
    ((Reg r),(Immediate (IntLiteral i))) ->
        addiEmitter r i 
    _ -> do
      r1Tmp <- forceToReg lhs
      r2Tmp <- forceToReg rhs
      invalidateCacheLine $ Reg r2Tmp
      emit $ RV_Add r2Tmp r2Tmp r1Tmp
      freeRegister r1Tmp
      pure $ Reg r2Tmp
    where addiEmitter r i = do
            emit $ RV_Addi r r i
            pure $ Reg r  

  codegenFuncCall :: String -> State (CodegenState RiscVInst) ()
  codegenFuncCall funcName =do
    funcSignature <- use (#knowFuncDef % at funcName)
    case funcSignature of
      Just (FuncTypeSignature argsTypes _retType) -> do
        vStack <- use #virtualStack
        let argsCount = (length argsTypes)
        when (length vStack < argsCount)
          $ error
          $ "Stack underflow: not enough args to call function "
              ++ funcName
              ++ " expected "
              ++ show argsCount
              ++ " got "
              ++ show (length vStack)
        let (args, stackRest) = splitAt (length argsTypes) vStack
        #virtualStack .= stackRest
        let argsInOrder = reverse args
        let (regArgs, memArgs) = splitAt 8 argsInOrder
        forM_ (zip ([0 ..] :: [Int]) regArgs) handleRegArgs
        handleMemArgs memArgs
        emitCall funcName
        restoreSp $ length memArgs
        #virtualStack %= (Reg (Register "a0" GeneralPurpose) :)
      Nothing -> error "Tries to call unknown function"
    where
      handleRegArgs (stackOffset, vStackItem) = do
        let argReg = (Register ("a" ++ show stackOffset) GeneralPurpose)
        case vStackItem of
          Immediate (IntLiteral i) -> loadImmediate i argReg
          Reg r -> do
            emitMove argReg r
            freeRegister r
          Spilled offset -> do
            emit $ RV_Ld argReg rvSpRegister offset
            freeHwStackOffset offset
          _ -> error "Unsupported type - only int Literals supported right now"
      handleMemArgs memArgs =
        let memArgCount = length memArgs
         in when (memArgCount > 0) $ do
              let argsBytes = alignTo rvSpAlignment (memArgCount * regSize)
              forM_ (zip ([0 ..] :: [Int]) memArgs) $ \(i, arg) ->
                pushToPhysStack (-argsBytes + (i * regSize)) arg
              emit $ bumpSp $ -argsBytes
      pushToPhysStack hwStackOffset toPush = do
        regToPush <- forceToReg toPush
        emit $ RV_Sd regToPush rvSpRegister hwStackOffset
        freeRegister regToPush
      restoreSp memArgsCount =
        when (memArgsCount > 0) $ emit $ bumpSp $ memArgsCount * regSize

  loadImmediate :: Immediate -> Register -> State (CodegenState RiscVInst) ()
  loadImmediate imm reg = do
    #cache % at (Const imm) .= Just (Reg reg)
    emit $ RV_Li reg imm

  emitLoad :: Register -> Register -> Immediate -> RiscVInst
  emitLoad target src offset  =
    RV_Ld target src offset
  
  emitStore :: Register -> Register -> Immediate -> RiscVInst
  emitStore src targetAddr offset =
    RV_Sd src targetAddr offset
  
  emitAddi :: Register -> Register -> Immediate -> RiscVInst
  emitAddi target r i = 
    RV_Addi target r i

  emitMove :: Register -> Register -> State (CodegenState RiscVInst) ()
  emitMove r1 r2 =
    if r1 /= r2
      then emit $ Rv_Mv r1 r2
      else return ()
  
  emitCall :: String -> State (CodegenState RiscVInst) ()
  emitCall callee = do
    invalidateCache
    emit $ RV_Call callee

  spRegister :: Proxy RiscVInst -> Register
  spRegister _ = rvSpRegister

  raRegister :: Proxy RiscVInst -> Register
  raRegister _ = rvRaRegister

  spAlignment :: Proxy RiscVInst -> Int
  spAlignment _ = 16

  extraFrameSlotsCount :: Proxy RiscVInst -> Int
  extraFrameSlotsCount _ = 1

  registerSize :: Proxy RiscVInst -> Int
  registerSize _ = 8


instance RegisterAllocator RiscVInst where
  initialRegisterPool :: Proxy RiscVInst -> [Register]
  initialRegisterPool _ =  map (\n -> Register ("t" ++ show n) GeneralPurpose) ([0 .. 6] :: [Int])

class  RegisterAllocator target where
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
    when (length activeRegisters == 0)
      $ error
          "Error: run out of CPU register and there are also non to be spilled to memory" -- This should never happened
    let toSpill = last activeRegisters
    spillOffset <- allocateHwStackOffset
    invalidateCacheLine $ Reg toSpill
    #virtualStack %= map (spillRegisterHelper toSpill spillOffset)
    emit $ emitStore toSpill rvSpRegister spillOffset
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
  
  allocateHwStackOffset :: State (CodegenState target) HwStackOffset
  allocateHwStackOffset = do
    freeOffsets <- use #freeSpillOffsets
    case freeOffsets of
      (allocated:rest) -> do
        #freeSpillOffsets .= rest
        pure allocated
      [] -> do
        offset <- use #nextSpillOffset
        #nextSpillOffset %= (+ regSize)
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

  initialRegisterPool :: Proxy target -> [Register]


emit :: inst -> State (CodegenState inst) ()
emit inst = #emittedCode %= (inst :)

alignTo :: Int -> Int -> Int
alignTo alignment x = (x + (alignment - 1)) .&. (-alignment)