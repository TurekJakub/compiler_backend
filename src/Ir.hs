module Ir
  ( module Ir
  ) where

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
  deriving (Show)

data FunctionDef = FunctionDef
  { name :: String
  , body :: [IrToken]
  } deriving (Show)

type Program = [FunctionDef]

-- Maybe later :)
data BasicBlock = BasicBlock
  { instructions :: [IrToken]
  } deriving (Show)
