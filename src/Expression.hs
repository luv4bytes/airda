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
module Expression (expression) where

import qualified AST
import qualified Error
import qualified Lexer
import qualified ParserState

expression :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
expression [] = Left (Error.ParserExceptionSimple "Expected expression.")
expression [t]
  | Lexer.tokenType t == Lexer.NumericLiteral = Right (AST.Expression (AST.NumericLiteral (Lexer.tokenValue t)), [])
  | Lexer.tokenType t == Lexer.Identifier = Right (AST.Expression (AST.Identifier (Lexer.tokenValue t)), [])
  | Lexer.tokenType t == Lexer.ClosedParen = Right (AST.Epsilon, [])
  | otherwise = Right (AST.Epsilon, [t])
expression pstate@(t : rest@(tt : ts))
  | Lexer.tokenType t == Lexer.Identifier && Lexer.tokenType tt == Lexer.Plus =
      case expression ts of
        Left pe -> Left pe
        Right (rhs, state) ->
          Right
            ( AST.BinaryExpression
                ( AST.Expression (AST.Identifier (Lexer.tokenValue t))
                )
                (AST.Operator (Lexer.tokenValue tt))
                rhs,
              state
            ) -- TODO: Parenthesis in binary expressions.
  | Lexer.tokenType t == Lexer.Identifier = Right (AST.Expression (AST.Identifier (val t)), rest)
  | Lexer.tokenType t == Lexer.NumericLiteral = Right (AST.Expression (AST.NumericLiteral (val t)), rest)
  | Lexer.tokenType t == Lexer.OpenParen =
      case expression rest of
        Left pe -> Left pe
        Right (exprNode, state) ->
          case closedParen state of
            Left pe -> Left pe
            Right state -> Right (AST.Expression exprNode, state)
  | Lexer.tokenType t == Lexer.Minus =
      case expression rest of
        Left pe -> Left pe
        Right (exprNode, state) -> Right (AST.UnaryExpression (AST.Operator (val t)) exprNode, state)
  | otherwise =
      Left
        ( Error.ParserException
            { Error.pexMessage = "Invalid expression '" ++ Lexer.tokenValue t ++ "'.",
              Error.pexErrCode = Error.errInvalidToken,
              Error.pexLineNum = Just (Lexer.tokenLineNum t),
              Error.pexColNum = Just (Lexer.tokenColumn t),
              Error.pexFileName = Lexer.fileName t
            }
        )
  where
    val = Lexer.tokenValue

    closedParen :: ParserState.ParserState -> Either Error.ParserException ParserState.ParserState
    closedParen [] = Left (Error.ParserExceptionSimple "Expected closed parenthesis.")
    closedParen pstate@(t : ts)
      | Lexer.tokenType t == Lexer.ClosedParen = Right ts
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected closed parenthesis.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )