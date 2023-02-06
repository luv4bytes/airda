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

-- | Expression contains functions to evaluate expressions.
module Expression (expression) where

import qualified AST
import qualified Error
import qualified Lexer
import qualified ParserState

-- TODO: Unary operators

expression :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
expression [] = Left Error.syntaxError
expression [t]
  | tt t == Lexer.Identifier = Right (idExpr t, [])
  | tt t == Lexer.NumericLiteral = Right (numExpr t, [])
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
    tt = Lexer.tokenType
expression state@(t : r@(_t : ts))
  | tt t == Lexer.Identifier && tt _t `elem` [Lexer.Multiply, Lexer.Divide] = multExpr ts (op _t) (idExpr t)
  | tt t == Lexer.Identifier && tt _t `elem` [Lexer.Plus, Lexer.Minus] = addExpr ts (op _t) (idExpr t)
  | tt t == Lexer.Identifier = Right (idExpr t, r)
  | tt t == Lexer.NumericLiteral && tt _t `elem` [Lexer.Multiply, Lexer.Divide] = multExpr ts (op _t) (idExpr t)
  | tt t == Lexer.NumericLiteral && tt _t `elem` [Lexer.Plus, Lexer.Minus] = addExpr ts (op _t) (numExpr t)
  | tt t == Lexer.NumericLiteral = Right (numExpr t, r)
  | tt t == Lexer.Minus = unaExpr r (op _t)
  | tt t == Lexer.OpenParen =
      case expression r of
        Left pe -> Left pe
        Right (expr, state) ->
          case closedParen state of
            Left pe -> Left pe
            Right [] -> Left Error.syntaxError
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Plus} : sts) -> addExpr sts (op st) (AST.Expression expr)
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Minus} : sts) -> addExpr sts (op st) (AST.Expression expr)
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Multiply} : sts) -> multExpr sts (op st) (AST.Expression expr)
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Divide} : sts) -> multExpr sts (op st) (AST.Expression expr)
            Right state -> Right (AST.Expression expr, state)
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

-- | Unary expression.
unaExpr :: ParserState.ParserState -> AST.TreeNode -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
unaExpr state op = Left {} -- TODO: Unary expressions.

-- | Additive Expression
addExpr :: ParserState.ParserState -> AST.TreeNode -> AST.TreeNode -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
addExpr [] _ _ = Left Error.syntaxError
addExpr [t] op lhs
  | tt t == Lexer.Identifier = Right (AST.BinaryExpression lhs op (idExpr t), [])
  | tt t == Lexer.NumericLiteral = Right (AST.BinaryExpression lhs op (numExpr t), [])
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
addExpr state@(t : r@(_t : ts)) binOp lhs
  | tt t == Lexer.Identifier && tt _t `elem` [Lexer.Plus, Lexer.Minus] =
      case addExpr ts (op _t) (AST.BinaryExpression lhs binOp (idExpr t)) of
        Left pe -> Left pe
        Right (e, s) -> Right (e, s)
  | tt t == Lexer.Identifier && tt _t `elem` [Lexer.Multiply, Lexer.Divide] =
      case multExpr ts (op _t) (idExpr t) of
        Left pe -> Left pe
        Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.Identifier = Right (AST.BinaryExpression lhs binOp (idExpr t), r)
  | tt t == Lexer.NumericLiteral && tt _t `elem` [Lexer.Plus, Lexer.Minus] =
      case addExpr ts (op _t) (AST.BinaryExpression lhs binOp (numExpr t)) of
        Left pe -> Left pe
        Right (ae, s) -> Right (ae, s)
  | tt t == Lexer.NumericLiteral && tt _t `elem` [Lexer.Multiply, Lexer.Divide] =
      case multExpr ts (op _t) (idExpr t) of
        Left pe -> Left pe
        Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.NumericLiteral = Right (AST.BinaryExpression lhs binOp (numExpr t), r)
  | tt t == Lexer.OpenParen =
      case expression r of
        Left pe -> Left pe
        Right (expr, state) ->
          case closedParen state of
            Left pe -> Left pe
            Right [] -> Left Error.syntaxError
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Plus} : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp (AST.Expression expr))
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Minus} : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp (AST.Expression expr))
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Multiply} : sts) ->
              case multExpr sts (op st) expr of
                Left pe -> Left pe
                Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Divide} : sts) ->
              case multExpr sts (op st) expr of
                Left pe -> Left pe
                Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
            Right state -> Right (AST.BinaryExpression lhs binOp (AST.Expression expr), state)
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

-- | Multiplicative Expression
multExpr :: ParserState.ParserState -> AST.TreeNode -> AST.TreeNode -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
multExpr [] _ _ = Left Error.syntaxError
multExpr [t] op lhs
  | tt t == Lexer.Identifier = Right (AST.BinaryExpression lhs op (idExpr t), [])
  | tt t == Lexer.NumericLiteral = Right (AST.BinaryExpression lhs op (numExpr t), [])
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
multExpr state@(t : r@(_t : ts)) binOp lhs
  | tt t == Lexer.Identifier && tt _t `elem` [Lexer.Multiply, Lexer.Divide] =
      case multExpr ts (op _t) (AST.BinaryExpression lhs binOp (idExpr t)) of
        Left pe -> Left pe
        Right (e, s) -> Right (e, s)
  | tt t == Lexer.Identifier && tt _t `elem` [Lexer.Plus, Lexer.Minus] =
      case addExpr ts (op _t) (AST.BinaryExpression lhs binOp (idExpr t)) of
        Left pe -> Left pe
        Right (e, s) -> Right (e, s)
  | tt t == Lexer.Identifier = Right (AST.BinaryExpression lhs binOp (idExpr t), r)
  | tt t == Lexer.NumericLiteral && tt _t `elem` [Lexer.Multiply, Lexer.Divide] =
      case multExpr ts (op _t) (AST.BinaryExpression lhs binOp (numExpr t)) of
        Left pe -> Left pe
        Right (e, s) -> Right (e, s)
  | tt t == Lexer.NumericLiteral && tt _t `elem` [Lexer.Plus, Lexer.Minus] =
      case addExpr ts (op _t) (AST.BinaryExpression lhs binOp (idExpr t)) of
        Left pe -> Left pe
        Right (e, s) -> Right (e, s)
  | tt t == Lexer.NumericLiteral = Right (AST.BinaryExpression lhs binOp (numExpr t), r)
  | tt t == Lexer.OpenParen =
      case expression r of
        Left pe -> Left pe
        Right (expr, state) ->
          case closedParen state of
            Left pe -> Left pe
            Right [] -> Left Error.syntaxError
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Plus} : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp (AST.Expression expr))
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Minus} : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp (AST.Expression expr))
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Multiply} : sts) ->
              case multExpr sts (op st) (AST.BinaryExpression lhs binOp (AST.Expression expr)) of
                Left pe -> Left pe
                Right (e, s) -> Right (e, s)
            Right (st@Lexer.Token {Lexer.tokenType = Lexer.Divide} : sts) ->
              case multExpr sts (op st) (AST.BinaryExpression lhs binOp (AST.Expression expr)) of
                Left pe -> Left pe
                Right (e, s) -> Right (e, s)
            Right state -> Right (AST.BinaryExpression lhs binOp (AST.Expression expr), state)
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

idExpr :: Lexer.Token -> AST.TreeNode
idExpr t = AST.Expression (AST.Identifier (Lexer.tokenValue t))

numExpr :: Lexer.Token -> AST.TreeNode
numExpr t = AST.Expression (AST.NumericLiteral (Lexer.tokenValue t))

op :: Lexer.Token -> AST.TreeNode
op t = AST.Operator (Lexer.tokenValue t)

tv :: Lexer.Token -> String
tv = Lexer.tokenValue

tt :: Lexer.Token -> Lexer.TokenType
tt = Lexer.tokenType

closedParen :: ParserState.ParserState -> Either Error.ParserException ParserState.ParserState
closedParen [] = Left Error.syntaxError
closedParen (t : ts)
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