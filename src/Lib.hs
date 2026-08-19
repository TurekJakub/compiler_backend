{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Lib
  ( module Lib
  ) where

import Abi
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Generics (Generic)
import Ir
import Optics.State.Operators ((%=), (.=))

data VStackItem
  = Immediate Literal
  | Reg Register
  | Spilled HwStackOffset
  deriving (Show, Eq)

data CacheKey
  = Var VarName
  | Slot Int
  | Const Int
  deriving (Show, Eq, Ord)

type HwStackOffset = Int

data CodegenState inst = CodegenState
  { virtualStack :: [VStackItem]
  , freeRegisters :: [Register]
  , cache :: Map CacheKey VStackItem
  , localVars :: Map VarName HwStackOffset
  , knowFuncDef :: Map String FuncTypeSignature
  , emittedCode :: [inst]
  , freeSpillOffsets :: [HwStackOffset]
  , nextSpillOffset :: HwStackOffset
  , blockStackStates :: Map LabelName [VStackItem]
  } deriving (Show, Generic)

invalidateCacheLine :: VStackItem -> State (CodegenState inst) ()
invalidateCacheLine invalLine = do
  #cache %= Map.filter (\line -> line /= invalLine)

invalidateCache :: State (CodegenState inst) ()
invalidateCache = #cache .= Map.empty

freeRegister :: Register -> State (CodegenState inst) ()
freeRegister reg = do
  #freeRegisters %= (reg :)
  invalidateCacheLine (Reg reg)
