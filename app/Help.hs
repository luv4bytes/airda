{--
MIT License

Copyright (c) 2023 Lukas Pfeifer

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
--}

module Help where

import Control.Monad ()
import GHC.IO.Device (IODevice (isTerminal))
import GHC.IO.FD (stdout)
import System.Exit (exitFailure)
import Text.Printf (printf)

-- | Prints the help for airda.
printHelp :: IO ()
printHelp = do
  isTerm <- isTerminal stdout
  if isTerm
    then printTerminalHelp
    else printNormalHelp
  where
    printTerminalHelp :: IO ()
    printTerminalHelp =
      do
        printf "\ESC[32;1mUsage\ESC[0m: airda [options] file...\n"
        printf "Options:\n"
        printf "%-2s\ESC[94;1m%-25s\ESC[0m%-40s\n" "" "--show-tree" "Prints the generated AST to stdout."
        printf "%-2s\ESC[94;1m%-25s\ESC[0m%-40s\n" "" "--write-tree [file]" "Writes the generated AST to the given file."
        printf "%-2s\ESC[94;1m%-25s\ESC[0m%-40s\n" "" "--help" "Prints this help text."
        printf "%-2s\ESC[94;1m%-25s\ESC[0m%-40s\n" "" "--files {f1, ... fn}" "Specifies the files to interpret."
        printf "\n"

    printNormalHelp :: IO ()
    printNormalHelp =
      do
        printf "Usage: airda [options] file...\n"
        printf "Options:\n"
        printf "%-2s%-25s%-40s\n" "" "--show-tree" "Prints the generated AST to stdout."
        printf "%-2s%-25s%-40s\n" "" "--write-tree [file]" "Writes the generated AST to the given file."
        printf "%-2s%-25s%-40s\n" "" "--help" "Prints this help text."
        printf "%-2s%-25s%-40s\n" "" "--files {f1, ... fn}" "Specifies the files to interpret."
        printf "\n"