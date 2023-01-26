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

import qualified Error as ER
import qualified LexerTypes as LT
import qualified ParserTypes as PT
import UnaryExpression (unaryExpression)

expression :: PT.ParserState -> Either ER.ParserException (PT.TreeNode, PT.ParserState)
expression [] = Left (ER.ParserExceptionSimple "Expected expression.")
expression state@(t : ts)
  | LT.tokenType t == LT.NumericLiteral =
      Right
        ( PT.NumericLiteralNode (LT.tokenValue t),
          ts
        )
  | LT.tokenType t == LT.Minus = unaryExpression state
  | LT.tokenType t == LT.Identifier =
      Right
        ( PT.IdentifierNode (LT.tokenValue t),
          ts
        )
  | otherwise =
      Left
        ( ER.ParserException
            { ER.pexMessage = "Invalid expression '" ++ LT.tokenValue t ++ "'.",
              ER.pexErrCode = ER.errInvalidExpression,
              ER.pexLineNum = Just (LT.tokenLineNum t),
              ER.pexColNum = Just (LT.tokenColumn t),
              ER.pexFileName = LT.fileName t
            }
        )