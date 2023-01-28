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

-- | Contains functions and types for argument parsing.
module Args where

-- | Contains program arguments.
data Arguments = Arguments
  { -- | The parse tree is printed after parsing stage.
    showParseTree :: Bool,
    -- | Defines if the parse tree should be written to a file.
    writeParseTree :: Bool,
    -- | Contains the name of the file the parse tree is written to.
    parseTreeFile :: String,
    -- | Shows the airda help.
    showHelp :: Bool,
    -- | List of files to compile.
    files :: [String]
  }
  deriving (Show)

-- | Argument for showing the parse tree.
showParseTreeArg :: String
showParseTreeArg = "--show-tree"

-- | Argument for writing the parse tree to a file.
writeParseTreeArg :: String
writeParseTreeArg = "--write-tree"

-- | Argument for showing the airda help.
showHelpArg :: String
showHelpArg = "--help"

-- | Argument for following list of files to compile.
filesArg :: String
filesArg = "--files"

-- | Parses the given arguments and returns parsed arguments or nothing.
parseArgs :: [String] -> Arguments
parseArgs [] = Arguments False False "" False []
parseArgs args = parseArgs' args (Arguments False False "" False [])
  where
    parseArgs' :: [String] -> Arguments -> Arguments
    parseArgs' [] arg = arg
    parseArgs' [a] arg@(Arguments showParseTree writeParseTree parseTreeFile showHelp files)
      | a == showParseTreeArg = arg {showParseTree = True}
      | a == showHelpArg = arg {showHelp = True}
      | otherwise = arg
    parseArgs' (a : rest@(aa : as)) arg@(Arguments showParseTree writeParseTree parseTreeFile showHelp files)
      | a == showParseTreeArg = parseArgs' rest arg {showParseTree = True}
      | a == showHelpArg = parseArgs' rest arg {showHelp = True}
      | a == filesArg = arg {files = rest}
      | a == writeParseTreeArg =
          parseArgs' as arg {writeParseTree = True, parseTreeFile = aa}
      | otherwise = parseArgs' rest arg