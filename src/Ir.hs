{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Ir
  ( module Ir
  ) where

import GHC.Generics (Generic)

data Literal
  = IntLiteral (Int)
  | CharLiteral (Char)
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

data IrType
  = IntType
  | CharType
  | DoubleType
  | VoidType
  deriving (Show, Eq, Ord)

data FuncTypeSignature = FuncTypeSignature
  { argTypes :: [IrType]
  , returnType :: IrType
  } deriving (Show, Generic)

data FunctionPrototype = FunctionPrototype
  { name :: LabelName
  , signature :: FuncTypeSignature
  } deriving (Show, Generic)

data FunctionDef = FunctionDef
  { prototype :: FunctionPrototype
  , body :: [IrToken]
  } deriving (Show, Generic)

data GlobalDef = GlobalDef
  { globalName :: VarName
  , globalType :: IrType
  , initialVal :: Literal
  , const :: Bool
  }

data Definition
  = Function FunctionDef
  | Global GlobalDef

type Program = [Definition]

-- Maybe later :)
data BasicBlock = BasicBlock
  { instructions :: [IrToken]
  } deriving (Show)
