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
compFilesArg :: String
compFilesArg = "--files"

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
      | a == compFilesArg = arg {files = rest}
      | a == writeParseTreeArg =
          parseArgs' as arg {writeParseTree = True, parseTreeFile = aa}
      | otherwise = parseArgs' rest arg