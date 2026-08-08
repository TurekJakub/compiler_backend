{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Codegen (module Codegen) where
import Abi
import qualified Data.Map as Map
import Ir (IrToken (Add, Peek, IrLiteral, Label, ConditionalBranch, Branch), Literal (..))

import Data.Map (Map)
import Control.Monad.State

import Optics
import Optics.State.Operators ((%=),(.=))
import GHC.Generics (Generic)

data CacheKey = Slot Int | Const Int deriving (Show, Eq, Ord)

data VStackItem = Immediate Literal | Reg Register deriving(Show, Eq)

data CodegenState = CodegenState
  { virtualStack  :: [VStackItem]
  , freeRegisters :: [Register]
  , cache      :: Map CacheKey VStackItem
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
 
codegenToken Add = do
  vStack <- use #virtualStack
  case vStack  of
    (Reg r1 : Reg r2 : stackRest) -> do 
        freeRegister r1
        #virtualStack .= (Reg r2 : stackRest)
        invalidateCacheLine $ Reg r2
        emit (InstRV (RV_Add r2 r2 r1))
    (Immediate i1 : Immediate i2 : stackRest) -> 
      case addLiterals i1 i2 of
        Just litSum -> #virtualStack .= (Immediate litSum) : stackRest
        Nothing -> error "Tries to sum non numerical literals"
    (Reg r1 : Immediate (IntLiteral i1) : stackRest) -> do
        #virtualStack .= (Reg r1 : stackRest)
        invalidateCacheLine $ Reg r1
        emit (InstRV $ RV_Addi r1 r1 i1)
    (Immediate (IntLiteral i1) : Reg r1 : stackRest) -> do
        #virtualStack .= (Reg r1 : stackRest)
        invalidateCacheLine $ Reg r1
        emit (InstRV $ RV_Addi r1 r1 i1)
    l | length l < 2 -> error "Stack underflow: there is not enough values to compute sum"
    _ -> error "Cannot emit add code for non-numerical values"


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


codegenToken _ = return ()

codgen :: [IrToken] -> CodegenState -> [Inst]
codgen inputIr initState = 
  let compilation = mapM_ codegenToken inputIr in

  let codegenResult = execState compilation initState in
  
  reverse (emittedCode codegenResult)


emit :: Inst -> State CodegenState ()
emit inst = #emittedCode %= (inst :)


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

addLiterals :: Literal -> Literal -> Maybe Literal
addLiterals (IntLiteral a) (IntLiteral b) = Just (IntLiteral (a + b))
addLiterals _ _ = Nothing

