{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Codegen (module Codegen) where
import Abi
import qualified Data.Map as Map
import Ir (IrToken (Add, Peek, IrLiteral, Label, ConditionalBranch, Branch, Sub, Mul, FunctionCall), Literal (..), FuncTypeSignature (..))

import Data.Map (Map)
import Control.Monad.State

import Optics
import Optics.State.Operators ((%=),(.=))
import GHC.Generics (Generic)
import Control.Monad (when, forM_, zipWithM_, zipWithM)
import Data.Bits ((.&.))

data CacheKey = Slot Int | Const Int deriving (Show, Eq, Ord)

data VStackItem = Immediate Literal | Reg Register deriving(Show, Eq)

data CodegenState = CodegenState
  { virtualStack  :: [VStackItem]
  , freeRegisters :: [Register]
  , cache      :: Map CacheKey VStackItem
  , knowFuncDef :: Map String FuncTypeSignature
  , emittedCode   :: [Inst]
  } deriving (Show, Generic)


spRegister :: Register
spRegister = Register "sp" GeneralPurpose

zeroRegister :: Register
zeroRegister = Register "zero" GeneralPurpose

codegenToken :: IrToken -> State CodegenState ()
codegenToken (IrLiteral lit)  = #virtualStack %= (Immediate lit :)

codegenToken (Peek offset) = do
  cachedLine <- use (#cache % at (Slot offset))
  case cachedLine of 
    Just value -> 
      #virtualStack %= (value :)
    Nothing -> do
      freeRegs <- use #freeRegisters
      case freeRegs of
        (nextReg : restRegs) -> do
          #freeRegisters .= restRegs
          #virtualStack  %= (Reg nextReg :)
          #cache % at (Slot offset) .= Just (Reg nextReg)

          emit (InstRV (RV_Lw nextReg spRegister offset))
        [] -> error "Spill out of registers!"
 
codegenToken Add =
  let addiEmitter = \r1 i1 -> emit (InstRV $ RV_Addi r1 r1 i1) in
  let addDef = 
        BinOpDef
          { regRegInst    = \r1 r2 r3-> InstRV $ RV_Add r1 r2 r3
          , immediateFolding   = mulLiterals
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
          { regRegInst    = \r1 r2 r3-> InstRV $ Rv_Mulw r1 r2 r3
          , immediateFolding   = mulLiterals
          , regToImmInst  = \r1 i1 -> do emit (InstRV $ RV_Sbw r1 zeroRegister r1)
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
          { regRegInst    = \r1 r2 r3-> InstRV $ Rv_Mulw r1 r2 r3
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
      emit (InstRV (RV_Beq r zeroRegister lbl))

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

codegenToken (FunctionCall name) = do
  signature <- use (#knowFuncDef % at name)
  case signature of
    Just (FuncTypeSignature argsTypes retType) -> do
      vStack <- use #virtualStack
      let argsCount = (length argsTypes)
      when (length vStack < argsCount) $ error $ 
        "Stack underflow: not enough args to call function " ++ name ++ " expected " ++ show argsCount ++ " got " ++ show (length vStack)

      let (args, stackRest) = splitAt (length argsTypes) vStack
      #virtualStack .= stackRest

      let argsInOrder = reverse args
      let (regArgs, memArgs) = splitAt 8 argsInOrder

      forM_ (zip ([0..]::[Int]) regArgs) handleRegArgs
      
      handleMemArgs memArgs

      emitCall name

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
              let argsBytes = memArgCount * 8
              bumpSp (-argsBytes)

              forM_ (zip ([0..]::[Int]) memArgs) pushToPhysStack

          pushToPhysStack (vStackOffset, vStackItem) = do
            let physicalStackOffset = vStackOffset * 8
            regToPush <- case vStackItem of
                  Reg r -> return $ Just r
                  Immediate (IntLiteral i) -> do
                    tmp <- forceImmediateToReg i
                  
                    return $ Just tmp
                  _ -> return Nothing
            case regToPush of 
              Just reg -> do 
                emit (InstRV $ RV_Sd reg spRegister physicalStackOffset)
                freeRegister reg
              Nothing ->  error "Type not implemented yet"
          restoreSp memArgsCount = 
              when (memArgsCount > 0) $
                bumpSp $ memArgsCount * 8

codegenToken _ = return ()

codgen :: [IrToken] -> CodegenState -> [Inst]
codgen inputIr initState = 
  let compilation = mapM_ codegenToken inputIr in

  let codegenResult = execState compilation initState in
  
  reverse (emittedCode codegenResult)


emit :: Inst -> State CodegenState ()
emit inst = #emittedCode %= (inst :)

emitCall :: String -> State CodegenState ()
emitCall callee = do 
  invalidateCache 
  emit (InstRV $ RV_Call callee)

bumpSp :: Int -> State CodegenState ()
bumpSp bytes = 
  let bumpBy = if (mod bytes 16) == 0 then
        bytes
      else
        alignTo rvSpAlignment bytes
  in emit (InstRV $ RV_Addi spRegister spRegister bumpBy)

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
      case (def ^. #immediateFolding) i1 i2 of
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

