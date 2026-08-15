{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}

module Codegen
  ( module Codegen
  ) where

import Abi
import qualified Data.Map as Map
import Ir
  ( FuncTypeSignature(FuncTypeSignature, argTypes)
  , FunctionDef(body, prototype)
  , FunctionPrototype(name, signature)
  , IrToken(Add, Branch, ConditionalBranch, Div, Drop, Eq, FunctionCall, GetLocal,
            Gt, Gte, IrLiteral, Label, Lt, Lte, Mod, Mul, SetLocal, Sub)
  , Literal(CharLiteral, IntLiteral)
  , Program
  , VarName
  )

import Control.Monad.State
import Data.Map (Map)

import Control.Monad (forM_, when)
import Data.Bits ((.&.))
import Data.Containers.ListUtils (nubOrd)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=))

data CacheKey
  = Var VarName
  | Slot Int
  | Const Int
  deriving (Show, Eq, Ord)

data VStackItem
  = Immediate Literal
  | Reg Register
  | Spilled HwStackOffset
  deriving (Show, Eq)

type HwStackOffset = Int

data CodegenState = CodegenState
  { virtualStack :: [VStackItem]
  , freeRegisters :: [Register]
  , cache :: Map CacheKey VStackItem
  , localVars :: Map VarName HwStackOffset
  , knowFuncDef :: Map String FuncTypeSignature
  , emittedCode :: [Inst]
  , freeSpillOffsets :: [HwStackOffset]
  , nextSpillOffset :: HwStackOffset
  } deriving (Show, Generic)

codegenToken :: IrToken -> State CodegenState ()
codegenToken (IrLiteral lit) = #virtualStack %= (Immediate lit :)
codegenToken (GetLocal varName) = do
  cachedReg <- use $ #cache % at (Var varName)
  case cachedReg of
    Just c -> #virtualStack %= (c :)
    Nothing -> do
      var <- use (#localVars % at varName)
      case var of
        Just varOffset -> do
          allocated <- allocateRegister
          #virtualStack %= (Reg allocated :)
          #cache % at (Var varName) .= Just (Reg allocated)
          emit (InstRV (RV_Ld allocated rvSpRegister varOffset))
        Nothing ->
          error
            $ "Tries to get value of undeclared local variable with label '"
                ++ varName
                ++ "'"
codegenToken (SetLocal varName) = do
  vStack <- use #virtualStack
  case vStack of
    (value:stackRest) -> do
      #virtualStack .= stackRest
      locals <- use #localVars
      varOffset <-
        case Map.lookup varName locals of
          Just offset -> pure offset
          Nothing -> do
            offset <- allocateHwStackOffset
            #localVars % at varName .= Just offset
            pure offset
      valueReg <- forceToReg value
      emit $ InstRV (RV_Sd valueReg rvSpRegister varOffset)
      #cache % at (Var varName) .= Just (Reg valueReg)
    _ -> error "Stack underflow in setLocal"
codegenToken Add =
  let addiEmitter = \r1 i1 -> emit (InstRV $ RV_Addi r1 r1 i1)
   in let addDef =
            BinOpDef
              { regRegInst = \r1 r2 r3 -> InstRV $ RV_Add r1 r2 r3
              , immediateFolding = addLiterals
              , regToImmInst = addiEmitter
              , immToRegInst = addiEmitter
              , regOnlyInst = False
              , underflowErrMsg =
                  "Stack underflow: there is not enough values to compute sum"
              , generalErrMsg = "Tries to sum non numerical literals"
              }
       in codegenBinOpHelper addDef
codegenToken Sub =
  let subDef =
        BinOpDef
          { regRegInst = \r1 r2 r3 -> InstRV $ RV_Sub r1 r2 r3
          , immediateFolding = subLiterals
          , regToImmInst =
              \r1 i1 -> do
                emit (InstRV $ RV_Sub r1 rvZeroRegister r1)
                emit (InstRV $ RV_Addi r1 r1 i1)
          , immToRegInst = \r1 i1 -> emit (InstRV $ RV_Addi r1 r1 (-i1))
          , regOnlyInst = False
          , underflowErrMsg =
              "Stack underflow: there is not enough values to compute difference"
          , generalErrMsg = "Tries to subtract non numerical literals"
          }
   in codegenBinOpHelper subDef
codegenToken Mul =
  let mulDef =
        BinOpDef
          { regRegInst = \r1 r2 r3 -> InstRV $ Rv_Mul r1 r2 r3
          , immediateFolding = mulLiterals
          , regToImmInst = \_ _ -> emit (InstRV $ Rv_Nop)
          , immToRegInst = \_ _ -> emit (InstRV $ Rv_Nop)
          , regOnlyInst = True
          , underflowErrMsg =
              "Stack underflow: there is not enough values to compute product"
          , generalErrMsg = "Tries to multiply non numerical literals"
          }
   in codegenBinOpHelper mulDef
codegenToken (Label label) = do
  blockChangeHelper
  emit (InstRV (RV_Label label))
codegenToken (Branch label) = do
  blockChangeHelper
  emit (InstRV (RV_J label))
codegenToken (ConditionalBranch lbl) = do
  vStack <- use #virtualStack
  case vStack of
    (Reg r:[]) -> do
      #virtualStack .= []
      freeRegister r
      invalidateCache
      emit (InstRV (RV_Beq r rvZeroRegister lbl))
    (Immediate (IntLiteral val):[]) -> do
      #virtualStack .= []
      if val == 0
        then do
          invalidateCache
          emit (InstRV (RV_J lbl))
        else return ()
    (Spilled offset:[]) -> do
      #virtualStack .= []
      tmp <- forceToReg $ Spilled offset
      invalidateCache
      emit (InstRV (RV_Beq tmp rvZeroRegister lbl))
      freeRegister tmp
    [] -> error "Stack underflow: nothing to evaluate for ConditionalBranch"
    _ -> error "Invalid stack value for ConditionalBranch"
codegenToken (FunctionCall funcName) = do
  funcSignature <- use (#knowFuncDef % at funcName)
  case funcSignature of
    Just (FuncTypeSignature argsTypes retType) -> do
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
          emit (InstRV $ Rv_Mv argReg r)
          freeRegister r
        Spilled offset -> do
          emit $ InstRV (RV_Ld argReg rvSpRegister offset)
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
      emit (InstRV $ RV_Sd regToPush rvSpRegister hwStackOffset)
      freeRegister regToPush
    restoreSp memArgsCount =
      when (memArgsCount > 0) $ emit $ bumpSp $ memArgsCount * regSize
codegenToken _ = return ()

{- Improve this in the future - for now only eight arguments passed via registers are supported
   TODO: add support for passing args via stack - should be fixed together with registers spilling implementation 
 -}
codegenFuncDefinition :: FunctionDef -> Map String FuncTypeSignature -> [Inst]
codegenFuncDefinition funcDef knowFuncDefs =
  let argsCount = length $ (view (#prototype % #signature % #argTypes) funcDef)
      frameSize = computeFrameSize funcDef knowFuncDefs
      initialCache =
        Map.fromList
          [ if i < 8
            then ( Var ("arg" ++ show i)
                 , Reg (Register ("a" ++ show i) GeneralPurpose))
            else ( Var ("arg" ++ show i)
                 , Spilled (frameSize + (i - 8) * regSize))
          | i <- [0 .. argsCount - 1]
          ]
      initState =
        CodegenState
          { virtualStack = [] -- Do not push arguments to stack right away, they will be lazy-loaded from cache on demand   
          , freeRegisters = rvTmpRegisters
          , cache = initialCache
          , emittedCode = []
          , knowFuncDef = knowFuncDefs
          , localVars = Map.empty
          , freeSpillOffsets = []
          , nextSpillOffset = 0
          }
      compilation = do
        mapM_ codegenToken (body funcDef)
        vStack <- use #virtualStack
        case vStack of
          [item] -> do
            tmp <- forceToReg item
            emit $ InstRV (Rv_Mv rvA0Register tmp)
            freeRegister tmp
          _ -> error $ "Function must leave exactly one value at stack"
      codegenResult = execState compilation initState
      raOffset = frameSize - regSize
      funcPrologue =
        [ InstRV $ RV_Label $ view (#prototype % #name) funcDef
        , bumpSp $ -frameSize
        , InstRV $ RV_Sd rvRaRegister rvSpRegister raOffset
        ]
      funcEpilog =
        [ InstRV $ RV_Ld rvRaRegister rvSpRegister raOffset
        , bumpSp frameSize
        , InstRV RV_Ret
        ]
   in funcPrologue ++ reverse (emittedCode codegenResult) ++ funcEpilog

codegen :: Program -> [Inst]
codegen program =
  let knowFuncDefs = collectFunctionDefs program
      codegenResult = map (flip codegenFuncDefinition knowFuncDefs) program
   in concat . reverse $ codegenResult

emit :: Inst -> State CodegenState ()
emit inst = #emittedCode %= (inst :)

emitCall :: String -> State CodegenState ()
emitCall callee = do
  invalidateCache
  emit (InstRV $ RV_Call callee)

bumpSp :: Int -> Inst
bumpSp bytes =
  let alignedBytes = alignTo rvSpAlignment (abs bytes)
   in let bumpBy =
            if bytes >= 0
              then alignedBytes
              else -alignedBytes
       in InstRV $ RV_Addi rvSpRegister rvSpRegister bumpBy

invalidateCacheLine :: VStackItem -> State CodegenState ()
invalidateCacheLine invalLine = do
  #cache %= Map.filter (\line -> line /= invalLine)

invalidateCache :: State CodegenState ()
invalidateCache = #cache .= Map.empty

freeRegister :: Register -> State CodegenState ()
freeRegister reg = do
  #freeRegisters %= (reg :)
  invalidateCacheLine (Reg reg)

allocateRegister :: State CodegenState Register
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

handleRegistersSpill :: State CodegenState Register
handleRegistersSpill = do
  activeRegisters <- getActiveRegisters
  when (length activeRegisters == 0)
    $ error
        "Error: run out of CPU register and there are also non to be spilled to memory" -- This should never happened
  let toSpill = last activeRegisters
  spillOffset <- allocateHwStackOffset
  invalidateCacheLine $ Reg toSpill
  #virtualStack %= map (spillRegisterHelper toSpill spillOffset)
  emit $ InstRV (RV_Sd toSpill rvSpRegister spillOffset)
  pure toSpill
  where
    spillRegisterHelper toSpill offset =
      \item ->
        if item == (Reg toSpill)
          then Spilled offset
          else item

allocateHwStackOffset :: State CodegenState HwStackOffset
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

freeHwStackOffset :: HwStackOffset -> State CodegenState ()
freeHwStackOffset offset = do
  vStack <- use #virtualStack
  let isReferenced =
        any
          (\case
             Spilled o -> o == offset
             _ -> False)
          vStack
  when (not isReferenced) $ #freeSpillOffsets %= (offset :)

getActiveRegisters :: State CodegenState [Register]
getActiveRegisters = do
  vStack <- use #virtualStack
  pure [r | Reg r <- vStack]

getCachedInactiveRegisters :: State CodegenState [Register]
getCachedInactiveRegisters = do
  cached <- use #cache
  activeRegisters <- getActiveRegisters
  pure $ [r | (_, Reg r) <- Map.toList cached, r `notElem` activeRegisters]

tryToFreeInactiveRegister :: State CodegenState (Maybe Register)
tryToFreeInactiveRegister = do
  inCacheOnly <- getCachedInactiveRegisters
  case inCacheOnly of
    (toFree:_) -> do
      invalidateCacheLine $ Reg toFree
      pure $ Just toFree
    [] -> pure $ Nothing

{- Enforce strict empty stack on basic block change invariant for now
   TODO: implement more mature solution that would required only same hight and values 'compatibility' -}
blockChangeHelper :: State CodegenState ()
blockChangeHelper = do
  vStack <- use #virtualStack
  if length vStack > 0
    then error "Stack must be empty when changing block"
    else invalidateCache

forceToReg :: VStackItem -> State CodegenState Register
forceToReg (Immediate (IntLiteral i)) = forceImmediateToReg i
forceToReg (Immediate (CharLiteral _)) =
  error "Only Int literals are supported yet :)"
forceToReg (Reg r) = pure r
forceToReg (Spilled spillOffset) = do
  reg <- allocateRegister
  emit (InstRV (RV_Ld reg rvSpRegister spillOffset))
  freeHwStackOffset spillOffset
  pure reg

forceImmediateToReg :: Immediate -> State CodegenState Register
forceImmediateToReg i = do
  cachedReg <- use (#cache % at (Const i))
  case cachedReg of
    Just (Reg r) -> pure r
    _ -> do
      loadTo <- allocateRegister
      loadImmediate i loadTo
      pure loadTo

loadImmediate :: Immediate -> Register -> State CodegenState ()
loadImmediate imm reg = do
  #cache % at (Const imm) .= Just (Reg reg)
  emit (InstRV $ RV_Li reg imm)

type CodegenBinOpRegAndIme = Register -> Immediate -> State CodegenState ()

data BinOpDef = BinOpDef
  { regRegInst :: Register -> Register -> Register -> Inst
  , immediateFolding :: Literal -> Literal -> Maybe Literal
  , regToImmInst :: CodegenBinOpRegAndIme
  , immToRegInst :: CodegenBinOpRegAndIme
  , regOnlyInst :: Bool
  , underflowErrMsg :: String
  , generalErrMsg :: String
  } deriving (Generic)

codegenBinOpHelper :: BinOpDef -> State CodegenState ()
codegenBinOpHelper def = do
  vStack <- use #virtualStack
  case vStack of
    (Immediate i1:Immediate i2:stackRest) ->
      case (def ^. #immediateFolding) i2 i1 of
        Just litSum -> #virtualStack .= (Immediate litSum) : stackRest
        Nothing -> error $ def ^. #generalErrMsg -- "Tries to sum non numerical literals"
    (Reg r1:Immediate (IntLiteral i1):stackRest) -> do
      let newStack = (\r -> Reg r1 : Reg r : stackRest)
      handleImmediate r1 i1 stackRest newStack (def ^. #regToImmInst)
    (Immediate (IntLiteral i1):Reg r1:stackRest) -> do
      let newStack = (\r -> Reg r : Reg r1 : stackRest)
      handleImmediate r1 i1 stackRest newStack (def ^. #immToRegInst)
    l
      | length l < 2 -> error $ def ^. #underflowErrMsg
    (r1:r2:stackRest) -> do
      r1Tmp <- forceToReg r1
      r2Tmp <- forceToReg r2
      #virtualStack .= (Reg r2Tmp : stackRest)
      invalidateCacheLine $ Reg r2Tmp
      emit $ (def ^. #regRegInst) r2Tmp r2Tmp r1Tmp
      freeRegister r1Tmp
    _ -> error $ def ^. #generalErrMsg
  where
    handleImmediate r1 i1 stackRest newStack instEmitter =
      if is12BitsImm i1 && not (def ^. #regOnlyInst)
        then do
          #virtualStack .= (Reg r1 : stackRest)
          invalidateCacheLine $ Reg r1
          instEmitter r1 i1
        else do
          r2 <- forceImmediateToReg i1
          #virtualStack .= newStack r2
          codegenBinOpHelper def

computeFrameSize :: FunctionDef -> Map String FuncTypeSignature -> Int
computeFrameSize func knownFuncDefs =
  let funcBody = func ^. #body
      localsCount = length $ collectLocals func
      maxStackDepth = computeMaxStackDepth funcBody 0 0
      spillSlotsCount = max 0 (maxStackDepth - length rvTmpRegisters)
      frameSlots = spillSlotsCount + localsCount + 1
   in alignTo rvSpAlignment (frameSlots * regSize)
  where
    computeMaxStackDepth [] _ peakDepth = peakDepth
    computeMaxStackDepth (h:ts) currDepth peakDepth =
      let depthChange = getTokenStackDepthDelta h
          newCurr = currDepth + depthChange
          newPeak = max peakDepth newCurr
       in computeMaxStackDepth ts newCurr newPeak
    getTokenStackDepthDelta =
      \case
        IrLiteral _ -> 1
        GetLocal _ -> 1
        SetLocal _ -> -1
        Drop -> -1
        Add -> -1
        Sub -> -1
        Mul -> -1
        Mod -> -1
        Lt -> -1
        Lte -> -1
        Gt -> -1
        Gte -> -1
        Eq -> -1
        Div -> -1
        Branch _ -> 0
        Label _ -> 0
        ConditionalBranch _ -> -1
        FunctionCall fname ->
          case Map.lookup fname knownFuncDefs of
            Just (FuncTypeSignature args _) -> 1 - length args
            Nothing -> 0

collectLocals :: FunctionDef -> [IrToken]
collectLocals def = nubOrd [SetLocal x | (SetLocal x) <- (def ^. #body)]

collectFunctionDefs :: Program -> Map String FuncTypeSignature
collectFunctionDefs program =
  Map.fromList
    [ (view (#prototype % #name) fn, view (#prototype % #signature) fn)
    | fn <- program
    ]

alignTo :: Int -> Int -> Int
alignTo alignment x = (x + (alignment - 1)) .&. (-alignment)

is12BitsImm :: Immediate -> Bool
is12BitsImm i = i >= -2048 && i <= 2047

addLiterals :: Literal -> Literal -> Maybe Literal
addLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral (a + b))
addLiterals _ _ = Nothing

subLiterals :: Literal -> Literal -> Maybe Literal
subLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral (a - b))
subLiterals _ _ = Nothing

mulLiterals :: Literal -> Literal -> Maybe Literal
mulLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral (a * b))
mulLiterals _ _ = Nothing
