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
import qualified Lexer
import qualified ParserState

expression :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
expression [] = Left (Error.ParserExceptionSimple "Expected expression.")
expression state =
  case term state of
    Left pe -> expression' state
    Right res -> Right res
  where
    expression' :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    expression' [] = Left (Error.ParserExceptionSimple "Expected expression.")
    expression' (t : ts)
      | Lexer.tokenType t == Lexer.OpenParen =
          case term ts of
            Left pe ->
              case expression' ts of
                Left pe' -> Left pe'
                Right (exprNode, pstate) ->
                  case closedParen pstate of
                    Left pe' -> Left pe'
                    Right (_, pstate) -> Right (AST.Expression exprNode, pstate)
            Right (termNode, pstate) ->
              case closedParen pstate of
                Left pe -> Left pe
                Right (_, pstate) -> Right (AST.Expression termNode, pstate)
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
      where
        closedParen :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
        closedParen [] = Left (Error.ParserExceptionSimple "Expected closed parenthesis.")
        closedParen (t : ts)
          | Lexer.tokenType t == Lexer.ClosedParen = Right (AST.Epsilon, ts)
          | otherwise =
              Left
                ( Error.ParserException
                    { Error.pexMessage = "Expected closed parenthesis.",
                      Error.pexErrCode = Error.errInvalidExpression,
                      Error.pexLineNum = Just (Lexer.tokenLineNum t),
                      Error.pexColNum = Just (Lexer.tokenColumn t),
                      Error.pexFileName = Lexer.fileName t
                    }
                )

    term :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    term [] = Left (Error.ParserExceptionSimple "Expected expression.")
    term state =
      case factor state of
        Left pe -> Left pe
        Right (node, pstate) -> Right (node, pstate)
    factor :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    factor [] = Left (Error.ParserExceptionSimple "Expected factor.")
    factor state@(t : ts)
      | Lexer.tokenType t == Lexer.Minus =
          factor' ts >>= \(node, pstate) ->
            Right
              ( AST.UnaryExpression (AST.UnaryOperator (Lexer.tokenValue t)) node,
                pstate
              )
      | otherwise = factor' state
      where
        factor' :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
        factor' [] = Left (Error.ParserExceptionSimple "Expected factor.")
        factor' (t : ts)
          | Lexer.tokenType t == Lexer.NumericLiteral = Right (AST.NumericLiteral (Lexer.tokenValue t), ts)
          | Lexer.tokenType t == Lexer.Identifier = Right (AST.Identifier (Lexer.tokenValue t), ts)
          | otherwise =
              Left
                ( Error.ParserException
                    { Error.pexMessage = "Invalid factor '" ++ Lexer.tokenValue t ++ "'.",
                      Error.pexErrCode = Error.errInvalidExpression,
                      Error.pexLineNum = Just (Lexer.tokenLineNum t),
                      Error.pexColNum = Just (Lexer.tokenColumn t),
                      Error.pexFileName = Lexer.fileName t
                    }
                )