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
    (null (Args.files parsedArgs))
    (print "Please provide at least one file." >> exitFailure)

  parseTrees <- parseFiles (Args.files parsedArgs)
  case parseTrees of
    Left pe -> print pe
    Right trees ->
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
                     writeTrees trees
                 )
          )
        when (Args.showParseTree parsedArgs) (showTrees trees)
      where
        treeFile = Args.parseTreeFile parsedArgs

        writeTrees :: AST.NodeList -> IO ()
        writeTrees [] = return ()
        writeTrees (root@(AST.Root nodes fileName) : ts) =
          let repr = Parser.treeRepr root
           in do
                appendFile treeFile repr
                writeTrees ts
        writeTrees (_ : ts) = writeTrees ts

        showTrees :: AST.NodeList -> IO ()
        showTrees [] = return ()
        showTrees (root@(AST.Root nodes fileName) : ts) =
          let repr = Parser.treeRepr root
           in do
                putStr repr
                showTrees ts
        showTrees (_ : ts) = showTrees ts

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

parseFiles :: [String] -> IO (Either Error.ParserException AST.NodeList)
parseFiles files = parseFiles' files [] 1
  where
    parseFiles' :: [String] -> AST.NodeList -> Int -> IO (Either Error.ParserException AST.NodeList)
    parseFiles' [] nodes _ = return (Right nodes)
    parseFiles' (f : fs) nodes index =
      do
        tokens <- readFileTokens f
        case Parser.parseTokens tokens f of
          Left pe -> return (Left pe)
          Right tn -> parseFiles' fs (nodes ++ [tn]) (index + 1)