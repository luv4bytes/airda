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

syntaxError :: ParserException
syntaxError = ParserExceptionSimple "Syntax error."