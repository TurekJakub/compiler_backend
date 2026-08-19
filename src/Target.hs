{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Target
  ( module Target
  ) where

import Abi
import Ir
import Lib

import Control.Monad.State
import Data.Map (Map)

import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=))

import qualified Data.Map as Map

class RegisterAllocator target =>
      InstSelector target
  where
  initCodegen ::
       Int -> Int -> Map String FuncTypeSignature -> CodegenState target
  forceToReg :: VStackItem -> State (CodegenState target) Register
  forceToReg (Immediate (IntLiteral i)) = forceImmediateToReg i
  forceToReg (Immediate (CharLiteral _)) =
    error "Only Int literals are supported yet :)"
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
  handleStackStatesMerge ::
       [VStackItem] -> [VStackItem] -> State (CodegenState target) ()
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
  codegenAdd ::
       VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenSub ::
       VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  codegenMul ::
       VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  emitLoad :: Register -> Register -> Immediate -> target
  emitStore :: Register -> Register -> Immediate -> target
  emitAddi :: Register -> Register -> Immediate -> target
  emitMove :: Register -> Register -> State (CodegenState target) ()
  emitCall :: String -> IrType -> State (CodegenState target) [VStackItem]
  emitLabel :: String -> target
  emitJump :: String -> target
  emitBranchIfEqual :: Register -> Register -> String -> target
  emitFuncProlog :: FunctionDef -> Int -> [target]
  emitFuncEpilog :: Int -> [target]
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
        emit $ emitLoad argReg rvSpRegister offset
        freeHwStackOffset offset
      _ -> error "Unsupported type - only int Literals supported right now"
  handleMemArgs :: [VStackItem] -> State (CodegenState target) ()
  handleMemArgs memArgs =
    let memArgCount = length memArgs
        regSize = registerSize @target
     in when (memArgCount > 0) $ do
          let argsBytes = alignTo rvSpAlignment (memArgCount * regSize)
          forM_ (zip ([0 ..] :: [Int]) memArgs) $ \(i, arg) ->
            pushToPhysStack (-argsBytes + (i * regSize)) arg
          emit $ bumpSp $ -argsBytes
  pushToPhysStack :: Int -> VStackItem -> State (CodegenState target) ()
  pushToPhysStack hwStackOffset toPush = do
    regToPush <- forceToReg toPush
    emit $ emitStore regToPush rvSpRegister hwStackOffset
    freeRegister regToPush
  restoreSp :: Int -> State (CodegenState target) ()
  restoreSp memArgsCount =
    when (memArgsCount > 0)
      $ emit
      $ bumpSp
      $ memArgsCount * registerSize @target
  bumpSp :: Int -> target
  bumpSp bytes =
    let alignedBytes = alignTo rvSpAlignment (abs bytes)
     in let bumpBy =
              if bytes >= 0
                then alignedBytes
                else -alignedBytes
         in emitAddi rvSpRegister rvSpRegister bumpBy
  spRegister :: Register
  raRegister :: Register
  spAlignment :: Int
  extraFrameSlotsCount :: Int
  registerSize :: Int
  funcArgumentsRegistersCount :: Int

instance InstSelector RiscVInst where
  initCodegen ::
       Int -> Int -> Map String FuncTypeSignature -> CodegenState RiscVInst
  initCodegen argsCount frameSize knowFuncDefs =
    let initialCache =
          Map.fromList
            [ if i < 8
              then ( Var ("arg" ++ show i)
                   , Reg (Register ("a" ++ show i) GeneralPurpose))
              else ( Var ("arg" ++ show i)
                   , Spilled (frameSize + (i - 8) * registerSize @RiscVInst))
            | i <- [0 .. argsCount - 1]
            ]
     in CodegenState
          { virtualStack = [] -- Do not push arguments to stack right away, they will be lazy-loaded from cache on demand   
          , freeRegisters = rvTmpRegisters
          , cache = initialCache
          , emittedCode = []
          , knowFuncDef = knowFuncDefs
          , localVars = Map.empty
          , freeSpillOffsets = []
          , nextSpillOffset = 0
          , blockStackStates = Map.empty
          }
  codegenAdd ::
       VStackItem -> VStackItem -> State (CodegenState RiscVInst) VStackItem
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
       VStackItem -> VStackItem -> State (CodegenState RiscVInst) VStackItem
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
       VStackItem -> VStackItem -> State (CodegenState RiscVInst) VStackItem
  codegenMul lhs rhs =
    let mulDef =
          BinOpDefinition
            { regToRegCodegen = \t r1 r2 -> emit $ Rv_Mul t r1 r2
            , regToImmCodegen = Nothing
            , immToRegCodegen = Nothing
            }
     in binCodgenOpHelper lhs rhs mulDef
  loadImmediate :: Immediate -> Register -> State (CodegenState RiscVInst) ()
  loadImmediate imm reg = do
    #cache % at (Const imm) .= Just (Reg reg)
    emit $ RV_Li reg imm
  emitLoad :: Register -> Register -> Immediate -> RiscVInst
  emitLoad target src offset = RV_Ld target src offset
  emitStore :: Register -> Register -> Immediate -> RiscVInst
  emitStore src targetAddr offset = RV_Sd src targetAddr offset
  emitAddi :: Register -> Register -> Immediate -> RiscVInst
  emitAddi target r i = RV_Addi target r i
  emitMove :: Register -> Register -> State (CodegenState RiscVInst) ()
  emitMove r1 r2 =
    if r1 /= r2
      then emit $ Rv_Mv r1 r2
      else return ()
  emitCall :: String -> IrType -> State (CodegenState RiscVInst) [VStackItem]
  emitCall callee retType = do
    invalidateCache
    emit $ RV_Call callee
    case retType of
      VoidType -> return []
      IntType -> return [Reg $ rvA0Register]
      _ -> error "Only void and int return types are supported right now"
  emitLabel :: String -> RiscVInst
  emitLabel labelName = RV_Label labelName
  emitJump :: String -> RiscVInst
  emitJump target = RV_J target
  emitBranchIfEqual :: Register -> Register -> String -> RiscVInst
  emitBranchIfEqual lhs rhs target = RV_Beq lhs rhs target
  emitFuncProlog :: FunctionDef -> Int -> [RiscVInst]
  emitFuncProlog funcDef funcFrameSize =
    let raOffset = getRaOffset funcFrameSize
     in [ RV_Label $ view (#prototype % #name) funcDef
        , bumpSp @RiscVInst (-funcFrameSize)
        , RV_Sd (raRegister @RiscVInst) (spRegister @RiscVInst) raOffset
        ]
  emitFuncEpilog :: Int -> [RiscVInst]
  emitFuncEpilog funcFrameSize =
    let raOffset = getRaOffset funcFrameSize
     in [ RV_Ld (raRegister @RiscVInst) (spRegister @RiscVInst) raOffset
        , bumpSp funcFrameSize
        , RV_Ret
        ]
  spRegister :: Register
  spRegister = rvSpRegister
  raRegister :: Register
  raRegister = rvRaRegister
  spAlignment :: Int
  spAlignment = 16
  extraFrameSlotsCount :: Int
  extraFrameSlotsCount = 1
  registerSize :: Int
  registerSize = 8
  funcArgumentsRegistersCount :: Int
  funcArgumentsRegistersCount = 8

instance RegisterAllocator RiscVInst where
  initialRegisterPool :: [Register]
  initialRegisterPool =
    map (\n -> Register ("t" ++ show n) GeneralPurpose) ([0 .. 6] :: [Int])

class RegisterAllocator target where
  allocateRegister ::
       InstSelector target => State (CodegenState target) Register
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
  handleRegistersSpill ::
       InstSelector target => State (CodegenState target) Register
  handleRegistersSpill = do
    activeRegisters <- getActiveRegisters
    when (length activeRegisters == 0)
      $ error
          "Error: run out of CPU register and there are also non to be spilled to memory"
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
                                                                                          -- This should never happened
  tryToFreeInactiveRegister :: State (CodegenState target) (Maybe Register)
  tryToFreeInactiveRegister = do
    inCacheOnly <- getCachedInactiveRegisters
    case inCacheOnly of
      (toFree:_) -> do
        invalidateCacheLine $ Reg toFree
        pure $ Just toFree
      [] -> pure $ Nothing
  allocateHwStackOffset ::
       InstSelector target => State (CodegenState target) HwStackOffset
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

emit :: inst -> State (CodegenState inst) ()
emit inst = #emittedCode %= (inst :)

alignTo :: Int -> Int -> Int
alignTo alignment x = (x + (alignment - 1)) .&. (-alignment)

is12BitsImm :: Immediate -> Bool
is12BitsImm i = i >= -2048 && i <= 2047

type ImmRegBinOpCodegen
  = Register -> Register -> Immediate -> State (CodegenState RiscVInst) ()

data BinOpDefinition = BinOpDefinition
  { regToRegCodegen :: Register -> Register -> Register -> State
                                                             (CodegenState
                                                                RiscVInst)
                                                             ()
  , immToRegCodegen :: Maybe ImmRegBinOpCodegen
  , regToImmCodegen :: Maybe ImmRegBinOpCodegen
  } deriving (Generic)

binCodgenOpHelper ::
     VStackItem
  -> VStackItem
  -> BinOpDefinition
  -> State (CodegenState RiscVInst) VStackItem
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

getRaOffset :: Int -> Int
getRaOffset funcFrameSize = funcFrameSize - registerSize @RiscVInst
