{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Demo where

import Asm.Asm (AsmPrinter, printAsmToFile)
import Asm.Riscv ()
import Codegen.Codegen (codegen)

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Target.Riscv.Rv32 ()
import Target.Riscv.Rv64 ()
import Target.Target (InstSelector)

runDemo ::
     forall a. (AsmPrinter a, InstSelector a)
  => String
  -> String
  -> IO ()
runDemo demoPath outputPath = do
  putStrLn $ "Generating RISC-V assembly for demo example: " ++ demoPath
  demoFile <- BLC.readFile demoPath
  case decode demoFile of
    Just s -> printAsmToFile (codegen @a s) outputPath
    Nothing -> error "failed to read demo example data from source file"
  putStrLn $ "Demo example RISC-V assembly successfully generated to " ++ outputPath
