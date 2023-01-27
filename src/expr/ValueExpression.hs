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

module ValueExpression where

import qualified AST
import qualified Error
import qualified Lexer
import qualified ParserState

valueExpression :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
valueExpression [] = Left (Error.ParserExceptionSimple "Expected expression.")
valueExpression (t : ts)
  | Lexer.tokenType t == Lexer.NumericLiteral =
      Right
        ( AST.NumericLiteral (Lexer.tokenValue t),
          ts
        )
  | Lexer.tokenType t == Lexer.Identifier =
      Right
        ( AST.Identifier (Lexer.tokenValue t),
          ts
        )
  | otherwise =
      Left
        ( Error.ParserException
            { Error.pexMessage = "Invalid expression '" ++ Lexer.tokenValue t ++ "'.",
              Error.pexErrCode = Error.errInvalidExpression,
              Error.pexLineNum = Just (Lexer.tokenLineNum t),
              Error.pexColNum = Just (Lexer.tokenColumn t),
              Error.pexFileName = Lexer.fileName t
            }
        )