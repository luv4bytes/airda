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

-- | Defines functions for parsing expressions.
module Expression where

import qualified AST
import qualified Error
import qualified LexerTypes
import qualified ParserState
import UnaryExpression (unaryExpression)

expression :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
expression [] = Left (Error.ParserExceptionSimple "Expected expression.")
expression state@(t : ts)
  | LexerTypes.tokenType t == LexerTypes.NumericLiteral =
      Right
        ( AST.NumericLiteral (LexerTypes.tokenValue t),
          ts
        )
  | LexerTypes.tokenType t == LexerTypes.Minus = unaryExpression state
  | LexerTypes.tokenType t == LexerTypes.Identifier =
      Right
        ( AST.Identifier (LexerTypes.tokenValue t),
          ts
        )
  | otherwise =
      Left
        ( Error.ParserException
            { Error.pexMessage = "Invalid expression '" ++ LexerTypes.tokenValue t ++ "'.",
              Error.pexErrCode = Error.errInvalidExpression,
              Error.pexLineNum = Just (LexerTypes.tokenLineNum t),
              Error.pexColNum = Just (LexerTypes.tokenColumn t),
              Error.pexFileName = LexerTypes.fileName t
            }
        )