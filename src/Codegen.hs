module Codegen (module Codegen) where

import Abi

import Data.Map (Map)

data CacheKey = Slot Int | Const Int deriving (Show, Eq, Ord)

data CodegenState = CodegenState
  { virtualStack  :: [Register]
  , freeRegisters :: [Register]
  , cache      :: Map CacheKey Register
  , emittedCode   :: [Inst]
  } deriving (Show)
