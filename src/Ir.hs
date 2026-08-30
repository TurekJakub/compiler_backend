{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ir
  ( module Ir
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Literal
  = IntLiteral (Int)
  | CharLiteral (Char)
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

type LabelName = String

type VarName = String

data IrToken
  = GetLocal VarName
  | SetLocal VarName
  | GetLocalAddr VarName
  | GetGlobal VarName
  | SetGlobal VarName
  | GetGlobalAddr VarName
  | Load
      { dataType :: IrType
      , offset :: Int
      }
  | Store
      { dataType :: IrType
      , offset :: Int
      }
  | Drop
  | IrLiteral Literal
  | FunctionCall
      { fncName :: String
      }
  | Label LabelName
  | Branch
      { label :: LabelName
      }
  | ConditionalBranch
      { label :: LabelName
      }
  | Add
  | Mul
  | Sub
  | Div
  | Mod
  | Lt
  | Lte
  | Gt
  | Gte
  | Eq
  | Not
  | PrintInt
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data IrType
  = IntType
  | CharType
  | DoubleType
  | VoidType
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data FuncTypeSignature = FuncTypeSignature
  { argTypes :: [IrType]
  , returnType :: IrType
  } deriving (Show, Generic, FromJSON, ToJSON)

data FunctionPrototype = FunctionPrototype
  { name :: LabelName
  , signature :: FuncTypeSignature
  } deriving (Show, Generic, FromJSON, ToJSON)

data FunctionDef = FunctionDef
  { prototype :: FunctionPrototype
  , body :: [IrToken]
  } deriving (Show, Generic, FromJSON, ToJSON)

data GlobalDef = GlobalDef
  { globalName :: VarName
  , globalType :: IrType
  , initialVal :: Literal
  , const :: Bool
  } deriving (Generic, FromJSON, ToJSON)

data Definition
  = Function FunctionDef
  | Global GlobalDef
  deriving (Generic, FromJSON, ToJSON)

type Program = [Definition]

-- Maybe later :)
data BasicBlock = BasicBlock
  { instructions :: [IrToken]
  } deriving (Show)
