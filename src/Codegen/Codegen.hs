{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Codegen.Codegen
  ( module Codegen.Codegen
  ) where

import Codegen.Common
import qualified Data.Map as Map
import Ir
  ( FuncTypeSignature(FuncTypeSignature, argTypes, returnType)
  , FunctionDef(body, prototype)
  , FunctionPrototype(name, signature)
  , IrToken(Add, Branch, ConditionalBranch, Div, Drop, Eq, FunctionCall, GetLocal,
            GetLocalAddr, Gt, Gte, IrLiteral, Label, Load, Lt, Lte, Mod, Mul, Not,
            SetLocal, Store, Sub)
  , IrType(VoidType)
  , LabelName
  , Literal(IntLiteral)
  , Program
  )
import Target.Target

import Control.Monad.State
import Data.Map (Map)

import Control.Monad (forM_, when)
import Data.Bool (bool)
import Data.Containers.ListUtils (nubOrd)
import GHC.Generics (Generic)
import Optics
import Optics.State.Operators ((%=), (.=))

codegenToken ::
     forall target. (InstSelector target, RegisterAllocator target)
  => IrToken
  -> State (CodegenState target) ()
codegenToken (IrLiteral lit) = #virtualStack %= (Immediate lit :)
codegenToken (GetLocal varName) = do
  cachedReg <- use $ #cache % at (Var varName)
  case cachedReg of
    Just c ->
      case c of
        Spilled _ -> do
          reg <- forceToReg c
          #virtualStack %= (Reg reg :)
        _ -> #virtualStack %= (c :)
    Nothing -> do
      var <- use (#localVars % at varName)
      case var of
        Just varOffset -> do
          allocated <- allocateRegister
          #virtualStack %= (Reg allocated :)
          #cache % at (Var varName) .= Just (Reg allocated)
          emit $ emitLoad allocated (spRegister @target) varOffset
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
      emit $ emitStore valueReg (spRegister @target) varOffset
      donNotCache <- use $ #notCachedLocals % contains varName
      when (not donNotCache) $ #cache % at (Var varName) .= Just (Reg valueReg)
    _ -> error "Stack underflow in setLocal"
codegenToken (GetLocalAddr varName) = do
  localVar <- use (#localVars % at varName)
  case localVar of
    Just offset -> do
      addr <- codegenGetLocalAddr offset
      #notCachedLocals % contains varName .= True --We need to forbid caching of values to which someone takes ptr
      #cache % at (Var varName) .= Nothing
      #virtualStack %= (addr :)
    Nothing ->
      error $ "Tries to take address of undeclared local variable " ++ varName
codegenToken (Load dataType offset) = do
  vStack <- use #virtualStack
  case vStack of
    (addr:stackRest) -> do
      loadedVal <- codegenLoad dataType offset addr
      #virtualStack .= (loadedVal : stackRest)
    _ -> error "Stack underflow: there is no base address on stack to emit load"
codegenToken (Store dataType offset) = do
  vStack <- use #virtualStack
  case vStack of
    (toStore:addr:stackRest) -> do
      codegenStore dataType offset toStore addr
      #virtualStack .= stackRest
    _ ->
      error
        "Stack underflow: store operation requires base address and value to store on stack"
codegenToken Add =
  let addDef =
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
codegenToken Div =
  let divDef =
        BinOpDef
          { opImplementation = codegenDiv
          , immediateFolding = divLiterals
          , underflowErrMsg =
              "Stack underflow: there is not enough values for division"
          , generalErrMsg = "Tries to divide non numerical literals"
          }
   in codegenBinOpHelper divDef
codegenToken Mod =
  let modDef =
        BinOpDef
          { opImplementation = codegenMod
          , immediateFolding = modLiterals
          , underflowErrMsg =
              "Stack underflow: there is not enough values to compute modulo"
          , generalErrMsg = "Tries to modulo non numerical literals"
          }
   in codegenBinOpHelper modDef
codegenToken Lt =
  let ltDef =
        BinOpDef
          { opImplementation = codegenLt
          , immediateFolding = ltLiterals
          , underflowErrMsg =
              "Stack underflow: there is not enough values to perform less than comparison"
          , generalErrMsg = "Tries to compare (less than) non numerical values"
          }
   in codegenBinOpHelper ltDef
codegenToken Lte =
  let lteDef =
        BinOpDef
          { opImplementation = codegenLte
          , immediateFolding = lteLiterals
          , underflowErrMsg =
              "Stack underflow: there is not enough values to perform less or equal than comparison"
          , generalErrMsg =
              "Tries to compare (less or equal than) non numerical values"
          }
   in codegenBinOpHelper lteDef
codegenToken Gt =
  let gtDef =
        BinOpDef
          { opImplementation = codegenGt
          , immediateFolding = gtLiterals
          , underflowErrMsg =
              "Stack underflow: there is not enough values to perform greater than comparison"
          , generalErrMsg =
              "Tries to compare (greater than) non numerical values"
          }
   in codegenBinOpHelper gtDef
codegenToken Gte =
  let gteDef =
        BinOpDef
          { opImplementation = codegenGte
          , immediateFolding = gteLiterals
          , underflowErrMsg =
              "Stack underflow: there is not enough values to perform greater or equal than comparison"
          , generalErrMsg =
              "Tries to compare (greater or equal than) non numerical values"
          }
   in codegenBinOpHelper gteDef
codegenToken Eq =
  let gteDef =
        BinOpDef
          { opImplementation = codegenEq
          , immediateFolding = eqLiterals
          , underflowErrMsg =
              "Stack underflow: there is not enough values to perform equality comparison"
          , generalErrMsg =
              "Tries to test equality of incomparable values values"
          }
   in codegenBinOpHelper gteDef
codegenToken Not = do
  vStack <- use #virtualStack
  case vStack of
    (Immediate (IntLiteral i):rest) -> do
      let notI =
            if i == 0
              then 1
              else 0
      #virtualStack .= ((Immediate $ IntLiteral notI) : rest)
    (toNegate:stackRest) -> do
      notVal <- codegenNot toNegate
      #virtualStack .= (notVal : stackRest)
    _ -> error "Stack underflow: can not perform logical not on empty stack"
codegenToken (Label labelName) = do
  blockChangeHelper labelName
  emit $ emitLabel labelName
codegenToken (Branch target) = do
  blockChangeHelper target
  emit $ emitJump target
  #virtualStack .= []
codegenToken (ConditionalBranch target) = do
  vStack <- use #virtualStack
  case vStack of
    (Immediate (IntLiteral val):rest) -> do
      #virtualStack .= rest
      if val == 0
        then do
          blockChangeHelper target
          emit $ emitJump target
        else return ()
    (top:rest) -> do
      #virtualStack .= rest
      codegenBranchIfZero top target
      blockChangeHelper target
    [] -> error "Stack underflow: nothing to evaluate for ConditionalBranch"
codegenToken (FunctionCall funcName) = do
  funcSignature <- use (#knowFuncDef % at funcName)
  case funcSignature of
    Just (FuncTypeSignature argsTypes retType) -> do
      vStack <- use #virtualStack
      let argsCount = length argsTypes
      when (length vStack < argsCount)
        $ error
        $ "Stack underflow: not enough args to call function "
            ++ funcName
            ++ " expected "
            ++ show argsCount
            ++ " got "
            ++ show (length vStack)
      onStackRegisters <- getStackRegisters
      let callerSaved = callerSavedRegisters @target
          toSave = filter (`elem` callerSaved) onStackRegisters
      toRestore <- saveCallerSaved toSave
      let (args, stackRest) = splitAt argsCount vStack
          argsRegsCount = funcArgumentsRegistersCount @target
          argsInOrder = reverse args
          (regArgs, memArgs) = splitAt argsRegsCount argsInOrder
      #virtualStack .= stackRest
      forM_ (zip ([0 ..] :: [Int]) regArgs) handleRegArgs
      handleMemArgs memArgs
      returnValue <- emitCall funcName retType
      #virtualStack %= (returnValue ++)
      restoreCallerSaved toRestore
      when (length memArgs > 0) $ restoreSp $ length memArgs
      #virtualStack %= (returnValue ++)
    Nothing -> error "Tries to call unknown function"
codegenToken _ = return ()

codegenFuncDefinition ::
     forall target. (InstSelector target, RegisterAllocator target)
  => FunctionDef
  -> Map String FuncTypeSignature
  -> [target]
codegenFuncDefinition funcDef knowFuncDefs =
  let argsCount = length $ (view (#prototype % #signature % #argTypes) funcDef)
      retType = view (#prototype % #signature % #returnType) funcDef
      frameSize = computeFrameSize @target funcDef knowFuncDefs
      initState = initCodegen argsCount frameSize knowFuncDefs
      compilation = do
        mapM_ codegenToken (body funcDef)
        vStack <- use #virtualStack
        case vStack of
          [item] -> do
            tmp <- forceToReg item
            case returnValueRegisters @target of
              (retReg:_) -> emitMove retReg tmp
              _ ->
                error
                  "There are no return value registers defined in target definition" -- This should never happened, implies backend target author error
            freeRegister tmp
          (h:rest) ->
            error
              $ "Function must leave exactly one value at stack, actual stack: "
                  ++ show (h : rest)
          [] ->
            when (retType /= VoidType)
              $ error "Function vit non void return type must return value"
      codegenResult = execState compilation initState
   in emitFuncProlog funcDef frameSize
        ++ reverse (emittedCode codegenResult)
        ++ emitFuncEpilog frameSize

codegen ::
     forall target. InstSelector target
  => Program
  -> [target]
codegen program =
  let knowFuncDefs = collectFunctionDefs program
      codegenResult = map (flip codegenFuncDefinition knowFuncDefs) program
   in concat . reverse $ codegenResult

blockChangeHelper ::
     InstSelector target => LabelName -> State (CodegenState target) ()
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

data BinOpDef target = BinOpDef
  { immediateFolding :: Literal -> Literal -> Maybe Literal
  , opImplementation :: VStackItem -> VStackItem -> State
                                                      (CodegenState target)
                                                      VStackItem
  , underflowErrMsg :: String
  , generalErrMsg :: String
  } deriving (Generic)

codegenBinOpHelper ::
     InstSelector target => BinOpDef target -> State (CodegenState target) ()
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

saveCallerSaved ::
     forall target. (InstSelector target, RegisterAllocator target)
  => [Register]
  -> State (CodegenState target) (Map Register Int)
saveCallerSaved toSave =
  foldM
    (\acc reg -> do
       offset <- allocateHwStackOffset
       emit $ emitStore reg (spRegister @target) offset
       pure (Map.insert reg offset acc))
    Map.empty
    toSave

restoreCallerSaved ::
     forall target. (InstSelector target, RegisterAllocator target)
  => Map Register Int
  -> State (CodegenState target) ()
restoreCallerSaved savedMap = forM_ (Map.toList savedMap) $ \(reg, offset) -> emit $ emitLoad reg (spRegister @target) offset

getStackRegisters ::
     forall target. (InstSelector target, RegisterAllocator target)
  => State (CodegenState target) [Register]
getStackRegisters = do
  vStack <- use #virtualStack
  pure [r | Reg r <- vStack]

computeFrameSize ::
     forall target. (InstSelector target, RegisterAllocator target)
  => FunctionDef
  -> Map String FuncTypeSignature
  -> Int
computeFrameSize func knownFuncDefs =
  let funcBody = func ^. #body
      localsCount = length $ collectLocals func
      maxStackDepth = computeMaxStackDepth funcBody 0 0
      spillSlotsCount = max 0 (maxStackDepth - length (initialRegisterPool @target))
      frameSlots = spillSlotsCount + localsCount + (extraFrameSlotsCount @target)
   in alignTo (spAlignment @target) (frameSlots * (registerSize @target))
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
        GetLocalAddr _ -> 1
        Load _ _ -> 0
        Store _ _ -> -2
        Drop -> -1
        Add -> -1
        Sub -> -1
        Mul -> -1
        Div -> -1
        Mod -> -1
        Lt -> -1
        Lte -> -1
        Gt -> -1
        Gte -> -1
        Eq -> -1
        Not -> 0
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

addLiterals :: Literal -> Literal -> Maybe Literal
addLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral $ a + b)
addLiterals _ _ = Nothing

subLiterals :: Literal -> Literal -> Maybe Literal
subLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral $ a - b)
subLiterals _ _ = Nothing

mulLiterals :: Literal -> Literal -> Maybe Literal
mulLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral $ a * b)
mulLiterals _ _ = Nothing

divLiterals :: Literal -> Literal -> Maybe Literal
divLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral $ a `div` b)
divLiterals _ _ = Nothing

modLiterals :: Literal -> Literal -> Maybe Literal
modLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral $ a `mod` b)
modLiterals _ _ = Nothing

ltLiterals :: Literal -> Literal -> Maybe Literal
ltLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral $ bool 0 1 $ a < b)
ltLiterals _ _ = Nothing

lteLiterals :: Literal -> Literal -> Maybe Literal
lteLiterals (IntLiteral a) (IntLiteral b) =
  Just (IntLiteral $ bool 0 1 $ a <= b)
lteLiterals _ _ = Nothing

gtLiterals :: Literal -> Literal -> Maybe Literal
gtLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral $ bool 0 1 $ a > b)
gtLiterals _ _ = Nothing

gteLiterals :: Literal -> Literal -> Maybe Literal
gteLiterals (IntLiteral a) (IntLiteral b) =
  Just (IntLiteral $ bool 0 1 $ a >= b)
gteLiterals _ _ = Nothing

eqLiterals :: Literal -> Literal -> Maybe Literal
eqLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral $ bool 0 1 $ a == b)
eqLiterals _ _ = Nothing
