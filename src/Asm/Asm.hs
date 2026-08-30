{-# LANGUAGE OverloadedStrings #-}

module Asm.Asm
  ( module Asm.Asm
  ) where

import Data.Text.Lazy.Builder (Builder, singleton, toLazyText)
import qualified Data.Text.Lazy.IO as TextIO

class AsmPrinter asmType where
  emitInstAssembly :: asmType -> Builder

emitAssembly :: AsmPrinter asmType => [asmType] -> Builder
emitAssembly insts = ".global _start\n.text\n" <> foldMap (\inst -> emitInstAssembly inst <> singleton '\n') insts

printAssembly :: AsmPrinter asmType => [asmType] -> IO ()
printAssembly insts = TextIO.putStr $ toLazyText (emitAssembly insts)

printAsmToFile :: AsmPrinter asmType => [asmType] -> String -> IO ()
printAsmToFile insts file = TextIO.writeFile file $ toLazyText (emitAssembly insts)
