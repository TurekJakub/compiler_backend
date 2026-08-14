{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Codegen (module Codegen) where
import Abi
import qualified Data.Map as Map
import Ir
    ( FuncTypeSignature(argTypes, FuncTypeSignature),
      FunctionDef(prototype, body),
      FunctionPrototype(signature, name),
      IrToken(FunctionCall, IrLiteral, GetLocal, SetLocal, Add,
              Sub, Mul, Label, Branch, ConditionalBranch),
      Literal(IntLiteral),
      Program,
      VarName ) 

import Data.Map (Map)
import Control.Monad.State

import Optics
import Optics.State.Operators ((%=),(.=))
import GHC.Generics (Generic)
import Control.Monad (when, forM_)
import Data.Bits ((.&.))

data CacheKey = Var VarName | Slot Int | Const Int deriving (Show, Eq, Ord)

data VStackItem = Immediate Literal | Reg Register deriving(Show, Eq)

type HwStackOffset = Int

data CodegenState = CodegenState
  { virtualStack  :: [VStackItem]
  , freeRegisters :: [Register]
  , cache      :: Map CacheKey VStackItem
  , localVars :: Map VarName HwStackOffset
  , knowFuncDef :: Map String FuncTypeSignature
  , emittedCode   :: [Inst]
  } deriving (Show, Generic)

codegenToken :: IrToken -> State CodegenState ()
codegenToken (IrLiteral lit)  = #virtualStack %= (Immediate lit :)

codegenToken (GetLocal varName) = do
  cachedReg <- use $ #cache % at  (Var varName) 
  case cachedReg of 
    Just c -> #virtualStack %= (c :)
    Nothing -> do 
      var <-  use (#localVars % at varName)
      case var of 
        Just varOffset -> do
          freeRegs <- use #freeRegisters
          case freeRegs of 
            (allocated:rest) ->do
              #freeRegisters .= rest
              #virtualStack %= (Reg allocated :)
              #cache % at (Var varName) .= Just (Reg allocated)
              emit (InstRV (RV_Ld allocated rvSpRegister varOffset))
            _ -> error "Register spilling not implemented yet" -- TODO: Implement this
        Nothing -> error $ "Tries to get value of undeclared local variable with label '" ++ varName ++ "'"

codegenToken (SetLocal varName) = do 
  vStack <- use #virtualStack
  case vStack of 
    (value : stackRest) -> do
      #virtualStack .= stackRest
      locals <- use #localVars 
      varOffset <- case Map.lookup varName locals of
        Just offset ->
          pure offset
        Nothing -> do
          let offset = Map.size locals
          #localVars % at varName .= Just offset
          pure offset
      valueReg <- case value of 
        Reg r ->
          pure $ Just r
        Immediate (IntLiteral i) -> do
          tmp <- forceImmediateToReg i
          pure $ Just tmp 
        _ -> pure Nothing
      case valueReg of
        Just r -> do
          emit $ InstRV (RV_Sd r rvSpRegister varOffset)
          #cache % at (Var varName) .= Just (Reg r)
        Nothing -> error "Type not implemented yet :)"
    _ -> error "Stack underflow in setLocal"
 
codegenToken Add =
  let addiEmitter = \r1 i1 -> emit (InstRV $ RV_Addi r1 r1 i1) in
  let addDef = 
        BinOpDef
          { regRegInst    = \r1 r2 r3-> InstRV $ RV_Add r1 r2 r3
          , immediateFolding   = addLiterals
          , regToImmInst  = addiEmitter
          , immToRegInst  = addiEmitter
          , regOnlyInst = False
          , underflowErrMsg  = "Stack underflow: there is not enough values to compute sum"
          , generalErrMsg = "Tries to sum non numerical literals"
          }
  in codegenBinOpHelper addDef

codegenToken Sub = 
  let subDef = 
        BinOpDef
          { regRegInst    = \r1 r2 r3-> InstRV $ RV_Sub r1 r2 r3
          , immediateFolding   = subLiterals
          , regToImmInst  = \r1 i1 -> do emit (InstRV $ RV_Sub r1 rvZeroRegister r1)
                                         emit (InstRV $ RV_Addi r1 r1 i1)
          , immToRegInst  = \r1 i1 -> emit (InstRV $ RV_Addi r1 r1 (-i1))
          , regOnlyInst = False
          , underflowErrMsg  = "Stack underflow: there is not enough values to compute difference"
          , generalErrMsg = "Tries to subtract non numerical literals"
          }
  in codegenBinOpHelper subDef

codegenToken Mul = 
  let mulDef = 
        BinOpDef
          { regRegInst    = \r1 r2 r3-> InstRV $ Rv_Mul r1 r2 r3
          , immediateFolding   = mulLiterals
          , regToImmInst  = \_ _ -> emit (InstRV $ Rv_Nop)
          , immToRegInst  = \_ _ -> emit (InstRV $ Rv_Nop)
          , regOnlyInst = True
          , underflowErrMsg  = "Stack underflow: there is not enough values to compute product"
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
    (Reg r : []) -> do
      #virtualStack .= []
      freeRegister r
      invalidateCache
      emit (InstRV (RV_Beq r rvZeroRegister lbl))

    (Immediate (IntLiteral val) : []) -> do
      #virtualStack .= []
      if val == 0
        then do
          invalidateCache
          emit (InstRV (RV_J lbl))
        else 
          return ()
          
    [] -> error "Stack underflow: nothing to evaluate for ConditionalBranch"
    _  -> error "Invalid stack value for ConditionalBranch"

codegenToken (FunctionCall funcName) = do
  funcSignature <- use (#knowFuncDef % at funcName)
  case funcSignature of
    Just (FuncTypeSignature argsTypes retType) -> do
      vStack <- use #virtualStack
      let argsCount = (length argsTypes)
      when (length vStack < argsCount) $ error $ 
        "Stack underflow: not enough args to call function " ++ funcName ++ " expected " ++ show argsCount ++ " got " ++ show (length vStack)

      let (args, stackRest) = splitAt (length argsTypes) vStack
      #virtualStack .= stackRest

      let argsInOrder = reverse args
      let (regArgs, memArgs) = splitAt 8 argsInOrder

      forM_ (zip ([0..]::[Int]) regArgs) handleRegArgs
      
      handleMemArgs memArgs

      emitCall funcName

      restoreSp $ length memArgs

      #virtualStack %= (Reg (Register "a0" GeneralPurpose) :)

    Nothing -> error "Tries to call unknown function"
    where handleRegArgs (stackOffset, vStackItem) = do
            freeRegs <- use #freeRegisters
            case vStackItem of
              Reg r  -> emit (InstRV $ Rv_Mv (Register ("a" ++ show stackOffset) GeneralPurpose) r)
              Immediate (IntLiteral i) -> loadImmediate i (Register ("a" ++ show stackOffset) GeneralPurpose) freeRegs
              _ -> return ()
          handleMemArgs memArgs =
            let memArgCount = length memArgs in
            when (memArgCount > 0) $ do
              let argsBytes = memArgCount * regSize
              emit $ bumpSp $ -argsBytes

              forM_ (zip ([0..]::[Int]) memArgs) pushToPhysStack

          pushToPhysStack (vStackOffset, vStackItem) = do
            let physicalStackOffset = vStackOffset * regSize
            regToPush <- case vStackItem of
                  Reg r -> return $ Just r
                  Immediate (IntLiteral i) -> do
                    tmp <- forceImmediateToReg i
                    return $ Just tmp
                  _ -> return Nothing
            case regToPush of 
              Just reg -> do 
                emit (InstRV $ RV_Sd reg rvSpRegister physicalStackOffset)
                freeRegister reg
              Nothing ->  error "Type not implemented yet"
          restoreSp memArgsCount = 
              when (memArgsCount > 0) $
                emit $ bumpSp $ memArgsCount * regSize

codegenToken _ = return ()

{- Improve this in the future - for now only eight arguments passed via registers are supported
   TODO: add support for passing args via stack - should be fixed together with registers spilling implementation 
 -}
codegenFuncDefinition :: FunctionDef ->  Map String FuncTypeSignature -> [Inst]
codegenFuncDefinition funcDef  knowFuncDefs = 
  let argsCount = length $ (view (#prototype % #signature % #argTypes) funcDef) 

      initialCache = Map.fromList[ (Var ("arg" ++ show i), Reg (Register ("a" ++ show i) GeneralPurpose)) | i <- take (min argsCount 8) ([0..] :: [Int])]

      initState = CodegenState
        { virtualStack  = [] -- Do not push arguments to stack right away, they will be lazy-loaded from cache on demand   
        , freeRegisters = rvTmpRegisters
        , cache         = initialCache
        , emittedCode   = []
        , knowFuncDef  = knowFuncDefs
        , localVars = Map.empty
        }
      
      compilation = do
        mapM_ codegenToken (body funcDef)

        vStack <- use #virtualStack
        {- This should be also reworked with register spilling -}
        case vStack of
          [item] ->
            case item of 
              Reg r -> do emit $ InstRV (Rv_Mv rvA0Register r )
                          freeRegister r
              Immediate (IntLiteral i) -> do 
                  tmp <- forceImmediateToReg i
                  emit (InstRV $ Rv_Mv rvA0Register tmp)
                  freeRegister tmp
              _ -> error "Only Int literals supported yet"
          _ -> error $ "Function must leave exactly one value at stack"

      codegenResult = execState compilation initState
    
      localsCount = Map.size $ codegenResult ^. #localVars 
      frameSize = alignTo rvSpAlignment (localsCount +1) * regSize
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

    in  funcPrologue ++ reverse (emittedCode codegenResult) ++ funcEpilog

codegen :: Program  -> [Inst]
codegen program = 
  let knowFuncDefs = collectFunctionDefs program 
      codegenResult = map (flip codegenFuncDefinition knowFuncDefs)  program
  in concat .  reverse $  codegenResult

emit :: Inst -> State CodegenState ()
emit inst = #emittedCode %= (inst :)

emitCall :: String -> State CodegenState ()
emitCall callee = do 
  invalidateCache 
  emit (InstRV $ RV_Call callee)

bumpSp :: Int -> Inst
bumpSp bytes = 
  let bumpBy = if (mod bytes 16) == 0 then
        bytes
      else
        alignTo rvSpAlignment bytes
  in InstRV $ RV_Addi rvSpRegister rvSpRegister bumpBy

invalidateCacheLine :: VStackItem -> State CodegenState ()
invalidateCacheLine invalLine =do 
  #cache %= Map.filter (\line -> line /= invalLine)

invalidateCache :: State CodegenState ()
invalidateCache = #cache .= Map.empty

freeRegister :: Register -> State CodegenState ()
freeRegister reg = do
  #freeRegisters %= (reg :)
  invalidateCacheLine(Reg reg)

{- Enforce strict empty stack on basic block change invariant for now
   TODO: implement more mature solution that would required only same hight and values 'compatibility' -}
blockChangeHelper :: State CodegenState ()
blockChangeHelper = do
  vStack <- use #virtualStack
  if length vStack > 0 then
    error "Stack must be empty when changing block"
  else
    invalidateCache

forceImmediateToReg :: Immediate -> State CodegenState Register
forceImmediateToReg i = do
    cachedReg <- use (#cache % at (Const i))
    case cachedReg of
      Just (Reg r) -> pure r
      _ -> do 
        freeRegs <- use #freeRegisters
        case freeRegs of
          (toAllocate : rest) -> do
           loadImmediate i toAllocate rest
           pure toAllocate
          _ -> error "Spill" -- TODO: Handle this

loadImmediate :: Immediate -> Register -> [Register] -> State CodegenState ()
loadImmediate imm reg regPool = do
   #freeRegisters .= regPool
   #cache % at (Const imm) .= Just (Reg reg)
   emit (InstRV $ RV_Li reg imm)

type CodegenBinOpRegAndIme = Register -> Immediate -> State CodegenState()
data BinOpDef = BinOpDef
  {
    regRegInst :: Register -> Register -> Register -> Inst,
    immediateFolding :: Literal -> Literal -> Maybe Literal,
    regToImmInst ::  CodegenBinOpRegAndIme,
    immToRegInst ::  CodegenBinOpRegAndIme,
    regOnlyInst :: Bool,
    underflowErrMsg :: String,
    generalErrMsg :: String
  }  deriving (Generic)

codegenBinOpHelper :: BinOpDef ->  State CodegenState ()
codegenBinOpHelper def = do
  vStack <- use #virtualStack
  case vStack  of
    (Reg r1 : Reg r2 : stackRest) -> do 
        freeRegister r1
        #virtualStack .= (Reg r2 : stackRest)
        invalidateCacheLine $ Reg r2
        emit $ (def ^. #regRegInst) r2 r2 r1
    (Immediate i1 : Immediate i2 : stackRest) -> 
      case (def ^. #immediateFolding) i2 i1 of
        Just litSum -> #virtualStack .= (Immediate litSum) : stackRest
        Nothing -> error $ def ^. #generalErrMsg -- "Tries to sum non numerical literals"
    (Reg r1 : Immediate (IntLiteral i1) : stackRest) -> do
        let newStack = (\r -> Reg r1 : Reg r : stackRest)
        handleImmediate r1 i1 stackRest newStack (def ^. #regToImmInst)
    (Immediate (IntLiteral i1) : Reg r1 : stackRest) -> do
      let newStack = (\r -> Reg r : Reg r1 : stackRest)
      handleImmediate r1 i1 stackRest newStack (def ^. #immToRegInst)
    l | length l < 2 -> error $ def ^. #underflowErrMsg
    _ -> error $ def ^. #generalErrMsg
  where handleImmediate r1 i1 stackRest newStack instEmitter = 
          if is12BitsImm i1 && not  (def ^. #regOnlyInst)
          then do
            #virtualStack .= (Reg r1 : stackRest)
            invalidateCacheLine $ Reg r1
            instEmitter r1  i1
          else do
            r2 <- forceImmediateToReg i1
            #virtualStack .= newStack r2
            codegenBinOpHelper def

collectFunctionDefs :: Program -> Map String FuncTypeSignature
collectFunctionDefs program =
  Map.fromList [ (view (#prototype % #name) fn, view (#prototype % #signature) fn) | fn <- program ]

alignTo :: Int -> Int -> Int
alignTo alignment x = (x + (alignment -1)) .&. (-alignment)

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
