{-# LANGUAGE OverloadedStrings #-}

module Asm.Asm
  ( module Asm.Asm
  ) where

import Data.Text.Lazy.Builder (Builder, singleton, toLazyText)
import qualified Data.Text.Lazy.IO as TextIO

class AsmPrinter asmType where
  emitInstAssembly :: asmType -> Builder

emitAssembly :: AsmPrinter asmType => [asmType] -> Builder
emitAssembly insts =
  ".globl main\n.text\n" <> foldMap (\inst -> emitInstAssembly inst <> singleton '\n') insts

printAssembly :: AsmPrinter asmType => [asmType] -> IO ()
printAssembly insts = TextIO.putStr $ toLazyText (emitAssembly insts)
