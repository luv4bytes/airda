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
copies or substantial portions of the SoftwarError.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARError.
--}

module Main where

import qualified AST
import qualified Args
import Control.Monad (when)
import Data.Maybe ()
import Data.Time (getCurrentTime, getCurrentTimeZone, getZonedTime)
import qualified Error
import qualified Help
import qualified Lexer
import qualified Parser
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs = Args.parseArgs args

  when (Args.showHelp parsedArgs) (Help.printHelp >> exitSuccess)
  when
    (null (Args.file parsedArgs))
    (print "Please provide at least one file." >> exitFailure)

  parseTree <- parseFile (Args.file parsedArgs)
  case parseTree of
    Left pe -> print pe
    Right tree ->
      do
        treeFileExists <- doesFileExist treeFile
        when
          (Args.writeParseTree parsedArgs)
          ( when
              treeFileExists
              (removeFile treeFile)
              >> ( do
                     now <- getZonedTime
                     appendFile treeFile (show now ++ "\n\n")
                     writeTree tree
                 )
          )
        when (Args.showParseTree parsedArgs) (showTree tree)
      where
        treeFile = Args.parseTreeFile parsedArgs

        writeTree :: AST.TreeNode -> IO ()
        writeTree root@(AST.Root _ _) =
          let repr = Parser.treeRepr root
           in do
                appendFile treeFile repr
        writeTree _ = return ()

        showTree :: AST.TreeNode -> IO ()
        showTree root@(AST.Root _ _) =
          let repr = Parser.treeRepr root
           in do
                putStr repr
        showTree _ = return ()

readFileTokens :: String -> IO Lexer.TokenList
readFileTokens fileName =
  do
    fileContent <- readFile fileName
    let fileLines = lines fileContent
    return (readTokens fileName fileLines)

readTokens :: String -> [String] -> Lexer.TokenList
readTokens _ [] = []
readTokens fileName fileLines = readTokens' fileLines 1
  where
    readTokens' :: [String] -> Int -> Lexer.TokenList
    readTokens' [] lineNum = []
    readTokens' (x : xs) lineNum =
      Lexer.tokenizeLine fileName x lineNum ++ readTokens' xs (lineNum + 1)

parseFile :: String -> IO (Either Error.ParserException AST.TreeNode)
parseFile file = do
  tokens <- readFileTokens file
  case Parser.parse tokens file of
    Left pe -> return (Left pe)
    Right tn -> return (Right tn)