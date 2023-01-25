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
    printTerminalHelp = return ()
    -- TODO:
    printNormalHelp :: IO ()
    printNormalHelp = return ()