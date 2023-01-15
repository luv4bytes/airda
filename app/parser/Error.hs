-- | Error contains functions for parser error messages and handling.
module Error where

errNoInput :: Int
errNoInput = 0

errInvalidToken :: Int
errInvalidToken = 1

errExpectedDifferentToken :: Int
errExpectedDifferentToken = 2

errInvalidExpression :: Int
errInvalidExpression = 3

-- | Defines a generic exception during parsing.
data ParserException
  = ParserException
      { pexMessage :: String,
        pexErrCode :: Int,
        pexLineNum :: Maybe Int,
        pexColNum :: Maybe Int,
        pexFileName :: String
      }
  | ParserExceptionSimple
      { pexMessage :: String
      }
  deriving (Show, Eq)