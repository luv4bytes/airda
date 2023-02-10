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

-- TOOD: Unary expressions

expression :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
expression [] = Left Error.syntaxError
expression [token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier})] = Right (idExpr token, [])
expression [token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral})] = Right (numExpr token, [])
expression [token] =
  Left
    ( Error.ParserException
        { Error.pexMessage = "Invalid expression '" ++ Lexer.tokenValue token ++ "'.",
          Error.pexErrCode = Error.errInvalidToken,
          Error.pexLineNum = Just (Lexer.tokenLineNum token),
          Error.pexColNum = Just (Lexer.tokenColumn token),
          Error.pexFileName = Lexer.fileName token
        }
    )
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : ts)) = addExpr ts (op next) (idExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : ts)) = addExpr ts (op next) (idExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : ts) = Right (idExpr token, ts)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : ts)) = addExpr ts (op next) (numExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : ts)) = addExpr ts (op next) (numExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : ts) = Right (numExpr token, ts)
expression state@((Lexer.Token {Lexer.tokenType = Lexer.OpenParen}) : ts) =
  case expression ts of
    Left pe -> Left pe
    Right (expr, state) ->
      case closedParen state of
        Left pe -> Left pe
        Right [] -> Left Error.syntaxError
        Right (st@Lexer.Token {Lexer.tokenType = Lexer.Plus} : sts) -> addExpr sts (op st) (AST.Expression expr)
        Right (st@Lexer.Token {Lexer.tokenType = Lexer.Minus} : sts) -> addExpr sts (op st) (AST.Expression expr)
        Right state -> Right (AST.Expression expr, state)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : rest@(next@(Lexer.Token {Lexer.tokenType = Lexer.OpenParen}) : ts)) =
  case expression rest of
    Left pe -> Left pe
    Right (e, st@Lexer.Token {Lexer.tokenType = Lexer.Plus} : sts) -> addExpr sts (op st) (AST.UnaryExpression (op token) (AST.Expression e))
    Right (e, st@Lexer.Token {Lexer.tokenType = Lexer.Minus} : sts) -> addExpr sts (op st) (AST.UnaryExpression (op token) (AST.Expression e))
    Right res -> Right res
expression state@(t : _) =
  Left
    ( Error.ParserException
        { Error.pexMessage = "Invalid expression '" ++ Lexer.tokenValue t ++ "'.",
          Error.pexErrCode = Error.errInvalidToken,
          Error.pexLineNum = Just (Lexer.tokenLineNum t),
          Error.pexColNum = Just (Lexer.tokenColumn t),
          Error.pexFileName = Lexer.fileName t
        }
    )

-- | Additive Expression
addExpr :: ParserState.ParserState -> AST.TreeNode -> AST.TreeNode -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
addExpr [] _ _ = Left Error.syntaxError
addExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier})] op lhs = Right (AST.BinaryExpression lhs op (idExpr token), [])
addExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral})] op lhs = Right (AST.BinaryExpression lhs op (numExpr token), [])
addExpr [t] op lhs =
  Left
    ( Error.ParserException
        { Error.pexMessage = "Invalid additive expression '" ++ Lexer.tokenValue t ++ "'.",
          Error.pexErrCode = Error.errInvalidToken,
          Error.pexLineNum = Just (Lexer.tokenLineNum t),
          Error.pexColNum = Just (Lexer.tokenColumn t),
          Error.pexFileName = Lexer.fileName t
        }
    )
addExpr state@(t : r@(_t : ts)) binOp lhs
  | tt t == Lexer.Identifier && isAddOp _t = addExpr ts (op _t) (AST.BinaryExpression lhs binOp (idExpr t))
  | tt t == Lexer.OpenParen =
      case expression state of
        Left pe -> Left pe
        Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.Identifier = Right (AST.BinaryExpression lhs binOp (idExpr t), r)
  | otherwise =
      Left
        ( Error.ParserException
            { Error.pexMessage = "Invalid additive expression '" ++ Lexer.tokenValue t ++ "'.",
              Error.pexErrCode = Error.errInvalidToken,
              Error.pexLineNum = Just (Lexer.tokenLineNum t),
              Error.pexColNum = Just (Lexer.tokenColumn t),
              Error.pexFileName = Lexer.fileName t
            }
        )

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

idExpr :: Lexer.Token -> AST.TreeNode
idExpr t = AST.Identifier (Lexer.tokenValue t)

numExpr :: Lexer.Token -> AST.TreeNode
numExpr t = AST.NumericLiteral (Lexer.tokenValue t)

op :: Lexer.Token -> AST.TreeNode
op t = AST.Operator (Lexer.tokenValue t)

tv :: Lexer.Token -> String
tv = Lexer.tokenValue

tt :: Lexer.Token -> Lexer.TokenType
tt = Lexer.tokenType

isMultOp :: Lexer.Token -> Bool
isMultOp t = tt t `elem` [Lexer.Multiply, Lexer.Divide]

isAddOp :: Lexer.Token -> Bool
isAddOp t = tt t `elem` [Lexer.Plus, Lexer.Minus]