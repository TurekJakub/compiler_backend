{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Codegen
  ( module Codegen
  ) where

import Abi
import Target
import Lib
import qualified Data.Map as Map
import Ir
  ( FuncTypeSignature(FuncTypeSignature, argTypes, returnType)
  , FunctionDef(body, prototype)
  , FunctionPrototype(name, signature)
  , IrToken(Add, Branch, ConditionalBranch, Div, Drop, Eq, FunctionCall, GetLocal,
            Gt, Gte, IrLiteral, Label, Lt, Lte, Mod, Mul, SetLocal, Sub)
  , IrType(VoidType)
  , LabelName
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
import Control.Exception.Backtrace (setBacktraceMechanismState)
import Data.Data (Proxy(Proxy))

codegenToken :: InstSelector target => IrToken -> State (CodegenState target) ()
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
          emit $ emitLoad allocated rvSpRegister varOffset
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
      emit $ emitStore valueReg rvSpRegister varOffset
      #cache % at (Var varName) .= Just (Reg valueReg)
    _ -> error "Stack underflow in setLocal"

codegenToken Add =
  let addiEmitter = \r1 i1 -> emit (InstRV $ RV_Addi r1 r1 i1)
   in let addDef =
            BinOpDef
              { opImplementation = codegenAdd 
              , immediateFolding = addLiterals
              , underflowErrMsg =
                  "Stack underflow: there is not enough values to compute sum"
              , generalErrMsg = "Tries to sum non numerical literals"
              }
       in codegenBinOpHelper addDef
codegenToken Sub =
  let subDef =
        BinOpDef
          { opImplementation = codegenSub
          , immediateFolding = subLiterals
          , underflowErrMsg =
              "Stack underflow: there is not enough values to compute difference"
          , generalErrMsg = "Tries to subtract non numerical literals"
          }
   in codegenBinOpHelper subDef
codegenToken Mul =
  let mulDef =
        BinOpDef
          { opImplementation = codegenMul
          , immediateFolding = mulLiterals
          , underflowErrMsg =
              "Stack underflow: there is not enough values to compute product"
          , generalErrMsg = "Tries to multiply non numerical literals"
          }
   in codegenBinOpHelper mulDef
codegenToken (Label target) = do
  blockChangeHelper target
  emit (InstRV (RV_Label target))
codegenToken (Branch target) = do
  blockChangeHelper target
  emit (InstRV (RV_J target))
  #virtualStack .= []
codegenToken (ConditionalBranch target) = do
  vStack <- use #virtualStack
  case vStack of
    (Reg r:rest) -> do
      #virtualStack .= rest
      freeRegister r
      blockChangeHelper target
      emit (InstRV (RV_Beq r rvZeroRegister target))
    (Immediate (IntLiteral val):rest) -> do
      #virtualStack .= rest
      if val == 0
        then do
          blockChangeHelper target
          emit (InstRV (RV_J target))
        else return ()
    (Spilled offset:rest) -> do
      #virtualStack .= rest
      tmp <- forceToReg $ Spilled offset
      blockChangeHelper target
      emit (InstRV (RV_Beq tmp rvZeroRegister target))
      freeRegister tmp
    [] -> error "Stack underflow: nothing to evaluate for ConditionalBranch"
    _ -> error "Invalid stack value for ConditionalBranch"
codegenToken (FunctionCall funcName) = do
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

codegenFuncDefinition :: InstSelector target => FunctionDef -> Map String FuncTypeSignature -> [target]
codegenFuncDefinition funcDef knowFuncDefs =
  let argsCount = length $ (view (#prototype % #signature % #argTypes) funcDef)
      retType = view (#prototype % #signature % #returnType) funcDef
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
          , blockStackStates = Map.empty
          }
      compilation = do
        mapM_ codegenToken (body funcDef)
        vStack <- use #virtualStack
        case vStack of
          [item] -> do
            tmp <- forceToReg item
            emitMove rvA0Register tmp
            freeRegister tmp
          (h:rest) ->
            error
              $ "Function must leave exactly one value at stack, actual stack: "
                  ++ show (h : rest)
          [] ->
            when (retType /= VoidType)
              $ error "Function vit non void return type must return value"
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

codegen :: forall target. InstSelector target => Program -> [target]
codegen program =
  let knowFuncDefs = collectFunctionDefs program
      codegenResult = map (flip codegenFuncDefinition knowFuncDefs) program
   in concat . reverse $ codegenResult

blockChangeHelper :: InstSelector target => LabelName -> State (CodegenState target) ()
blockChangeHelper target = do
  vStack <- use #virtualStack
  forcedVStack <- mapM (\item -> Reg <$> forceToReg item) vStack
  knownTargetState <- use (#blockStackStates % at target)
  invalidateCache
  case knownTargetState of
    Just targetState -> do
      when (length targetState /= length forcedVStack)
        $ error
        $ "Stack depth before and after jump must be the same "
            ++ show forcedVStack
            ++ " "
            ++ show targetState
      handleStackStatesMerge forcedVStack targetState
      #virtualStack .= targetState
    Nothing -> #blockStackStates % at target .= Just forcedVStack


type CodegenBinOpRegAndIme target = Register -> Immediate -> State (CodegenState target) ()

data BinOpDef target = BinOpDef
  {immediateFolding :: Literal -> Literal -> Maybe Literal
  , opImplementation ::  VStackItem -> VStackItem -> State (CodegenState target) VStackItem
  , underflowErrMsg :: String
  , generalErrMsg :: String
  } deriving (Generic)

codegenBinOpHelper :: InstSelector target => BinOpDef target -> State (CodegenState target) ()
codegenBinOpHelper def = do
  vStack <- use #virtualStack
  case vStack of
    (Immediate i1:Immediate i2:stackRest) ->
      case (def ^. #immediateFolding) i2 i1 of
        Just litSum -> #virtualStack .= (Immediate litSum) : stackRest
        Nothing -> error $ def ^. #generalErrMsg -- "Tries to sum non numerical literals"
    (r1:r2:stackRest) -> do
      res <- (def ^. #opImplementation) r1 r2
      #virtualStack .= (res : stackRest) 
      
    _ -> error $ def ^. #underflowErrMsg
  where
    handleImmediate r1 i1 stackRest newStack instEmitter =
      if is12BitsImm i1 -- && not (def ^. #regOnlyInst)
        then do
          #virtualStack .= (Reg r1 : stackRest)
          invalidateCacheLine $ Reg r1
          instEmitter r1 i1
        else do
          r2 <- forceImmediateToReg i1
          #virtualStack .= newStack r2
          codegenBinOpHelper def

computeFrameSize :: forall target. (InstSelector target, RegisterAllocator target) => FunctionDef -> Map String FuncTypeSignature -> Int
computeFrameSize func knownFuncDefs =
  let funcBody = func ^. #body
      localsCount = length $ collectLocals func
      maxStackDepth = computeMaxStackDepth funcBody 0 0
      spillSlotsCount = max 0 (maxStackDepth - length (initialRegisterPool $ Proxy @target))
      frameSlots = spillSlotsCount + localsCount +(extraFrameSlotsCount  $ Proxy @target)
   in alignTo (spAlignment $ Proxy @target)  (frameSlots * (registerSize $ Proxy @target))
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
