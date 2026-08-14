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
  | RV_Ld   Register Register Immediate  
  | RV_Li  Register Immediate
  | RV_Sd   Register Register Immediate
  | Rv_Addw  Register Register Register 
  | RV_Add  Register Register Register  
  | RV_Addiw Register Register Immediate 
  | RV_Addi Register Register Immediate 
  | RV_Slt  Register Register Register 
  | RV_Jal  Register LabelName          
  | RV_J    LabelName     
  | RV_Call LabelName        
  | RV_Ret       
  | RV_Beq  Register Register LabelName 
  | RV_Label LabelName
  | RV_Subw Register Register Register
  | RV_Sub Register Register Register
  | Rv_Mulw Register Register Register
  | Rv_Mul Register Register Register
  | Rv_Nop
  | Rv_Mv Register Register
  deriving (Show)

data X86Inst = TBD deriving(Show)

data Inst = InstRV RiscVInst | InstX86 X86Inst deriving (Show)

{- We target 64-bit RV64 right now -}
regSize :: Int
regSize = 8

rvSpAlignment :: Int
rvSpAlignment = 16

rvTmpRegisters :: [Register]
rvTmpRegisters =  map (\n -> Register ("t" ++ show n) GeneralPurpose) ([0 .. 6] :: [Int])

rvSpRegister :: Register
rvSpRegister = Register "sp" GeneralPurpose

rvRaRegister :: Register
rvRaRegister = Register "ra" GeneralPurpose

rvZeroRegister :: Register
rvZeroRegister = Register "zero" GeneralPurpose

rvA0Register:: Register
rvA0Register = Register "a0" GeneralPurpose