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
  } deriving(Show)
data FunctionPrototype = FunctionPrototype
  { name:: String,
    signature :: FuncTypeSignature
  } deriving (Show)

data FunctionDef = FunctionDef
  { prototype :: FunctionPrototype
  , body :: [IrToken]
  } deriving (Show)

type Program = [FunctionDef]

-- Maybe later :)
data BasicBlock = BasicBlock
  { instructions :: [IrToken]
  } deriving (Show)
