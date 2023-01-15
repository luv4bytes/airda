module Help where

import Control.Monad
import GHC.IO.Device
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