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

data IrToken
  = Poke
      { offset :: Int
      }
  | Peek
      { offset :: Int
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
  deriving (Show)

data IrType = IntType
  | CharType
  | DoubleType
  | VoidType deriving(Show)

data FuncTypeSignature =  FuncTypeSignature
  {
    argTypes :: [IrType],
    returnType :: IrType
  } deriving(Show, Generic)
data FunctionPrototype = FunctionPrototype
  { name:: LabelName,
    signature :: FuncTypeSignature
  } deriving (Show, Generic)

data FunctionDef = FunctionDef
  { prototype :: FunctionPrototype
  , body :: [IrToken]
  } deriving (Show, Generic)

type Program = [FunctionDef]

-- Maybe later :)
data BasicBlock = BasicBlock
  { instructions :: [IrToken]
  } deriving (Show)
