module Abi (module Abi) where 
import Ir (LabelName)

data RegisterType = GeneralPurpose deriving (Enum, Show, Eq)

data Register = Register {
    regName :: String,
    regType :: RegisterType
} deriving (Show, Eq)

type Immediate = Int

data RiscVInst
  = RV_Lw   Register Register Immediate  
  | RV_Li  Register Immediate
  | RV_Sd   Register Register Immediate  
  | RV_Add  Register Register Register  
  | RV_Addi Register Register Immediate 
  | RV_Slt  Register Register Register 
  | RV_Jal  Register LabelName          
  | RV_J    LabelName     
  | RV_Call LabelName               
  | RV_Beq  Register Register LabelName 
  | RV_Label LabelName
  | RV_Sbw Register Register Register
  | Rv_Mulw Register Register Register
  | Rv_Nop
  | Rv_Mv Register Register
  deriving (Show)

data X86Inst = TBD deriving(Show)

data Inst = InstRV RiscVInst | InstX86 X86Inst deriving (Show)

rvSpAlignment :: Int
rvSpAlignment = 16