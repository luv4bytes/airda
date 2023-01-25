module Main where

import qualified Args as A
import Control.Monad (when)
import Data.Maybe ()
import Data.Time (getCurrentTime, getCurrentTimeZone, getZonedTime)
import qualified Error as E
import qualified Help as H
import qualified Lexer as L
import qualified LexerTypes as LT
import qualified Parser as P
import qualified ParserTypes as PT
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs = A.parseArgs args

  when (A.showHelp parsedArgs) (H.printHelp >> exitSuccess)
  when
    (null (A.files parsedArgs))
    (print "Please provide at least one file." >> exitFailure)

  parseTrees <- parseFiles (A.files parsedArgs)
  case parseTrees of
    Left pe -> print pe
    Right trees ->
      do
        treeFileExists <- doesFileExist treeFile
        when
          (A.writeParseTree parsedArgs)
          ( when
              treeFileExists
              (removeFile treeFile)
              >> ( do
                     now <- getZonedTime
                     appendFile treeFile (show now ++ "\n\n")
                     writeTrees trees
                 )
          )
        when (A.showParseTree parsedArgs) (showTrees trees)
      where
        treeFile = A.parseTreeFile parsedArgs

        writeTrees :: PT.NodeList -> IO ()
        writeTrees [] = return ()
        writeTrees (root@(PT.TreeRoot nodes fileName) : ts) =
          let repr = P.treeRepr root
           in do
                appendFile treeFile repr
                writeTrees ts
        writeTrees (_ : ts) = writeTrees ts

        showTrees :: PT.NodeList -> IO ()
        showTrees [] = return ()
        showTrees (root@(PT.TreeRoot nodes fileName) : ts) =
          let repr = P.treeRepr root
           in do
                putStr repr
                showTrees ts
        showTrees (_ : ts) = showTrees ts

readFileTokens :: String -> IO LT.TokenList
readFileTokens fileName =
  do
    fileContent <- readFile fileName
    let fileLines = lines fileContent
    return (readTokens fileName fileLines)

readTokens :: String -> [String] -> LT.TokenList
readTokens _ [] = []
readTokens fileName fileLines = readTokens' fileLines 1
  where
    readTokens' :: [String] -> Int -> LT.TokenList
    readTokens' [] lineNum = []
    readTokens' (x : xs) lineNum =
      L.tokenizeLine fileName x lineNum ++ readTokens' xs (lineNum + 1)

parseFiles :: [String] -> IO (Either E.ParserException PT.NodeList)
parseFiles files = parseFiles' files [] 1
  where
    parseFiles' :: [String] -> PT.NodeList -> Int -> IO (Either E.ParserException PT.NodeList)
    parseFiles' [] nodes _ = return (Right nodes)
    parseFiles' (f : fs) nodes index =
      do
        tokens <- readFileTokens f
        case P.parseTokens tokens f of
          Left pe -> return (Left pe)
          Right tn -> parseFiles' fs (nodes ++ [tn]) (index + 1)