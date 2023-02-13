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
import qualified Primitives

-- TODO: Unary expressions inside addExpr, multExpr and exponExpr

expression :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
expression [] = Left Error.syntaxError
expression [token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier})] = Right (idExpr token, [])
expression [token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral})] = Right (numExpr token, [])
expression [token] = invalidExpr token
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : ts)) = addExpr ts (op next) (idExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : ts)) = addExpr ts (op next) (idExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : ts)) = multExpr ts (op next) (idExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : ts)) = multExpr ts (op next) (idExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Power}) : ts)) =
  case exponExpr ts (idExpr token) of
    Left pe -> Left pe
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : ts) -> addExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : ts) -> addExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : ts) -> multExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : ts) -> multExpr ts (op t) e
    Right res -> Right res
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : ts) = Right (idExpr token, ts)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : ts)) = addExpr ts (op next) (numExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : ts)) = addExpr ts (op next) (numExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : ts)) = multExpr ts (op next) (numExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : ts)) = multExpr ts (op next) (numExpr token)
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : (next@(Lexer.Token {Lexer.tokenType = Lexer.Power}) : ts)) =
  case exponExpr ts (numExpr token) of
    Left pe -> Left pe
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : ts) -> addExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : ts) -> addExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : ts) -> multExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : ts) -> multExpr ts (op t) e
    Right res -> Right res
expression state@(token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : ts) = Right (numExpr token, ts)
expression state@((Lexer.Token {Lexer.tokenType = Lexer.OpenParen}) : ts) =
  case parenExpr ts of
    Left pe -> Left pe
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : ts) -> addExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : ts) -> addExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : ts) -> multExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : ts) -> multExpr ts (op t) e
    Right res -> Right res
expression state@(t@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : ts) =
  case unarExpr ts (op t) of
    Left pe -> Left pe
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : ts) -> addExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : ts) -> addExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : ts) -> multExpr ts (op t) e
    Right (e, t@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : ts) -> multExpr ts (op t) e
    Right res -> Right res
expression state@(t : _) = invalidExpr t

-- | Exponantiation.
exponExpr :: ParserState.ParserState -> AST.TreeNode -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
exponExpr [] _ = Left Error.syntaxError
exponExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier})] lhs = Right (AST.BinaryExpression lhs (AST.Operator Primitives.sPower) (idExpr token), [])
exponExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral})] lhs = Right (AST.BinaryExpression lhs (AST.Operator Primitives.sPower) (numExpr token), [])
exponExpr [t] lhs = invalidExponExpr t
exponExpr state@(t@(Lexer.Token {Lexer.tokenType = Lexer.Identifier}) : r@(_t@(Lexer.Token {Lexer.tokenType = Lexer.Power}) : ts)) lhs =
  case exponExpr ts (idExpr t) of
    Left pe -> Left pe
    Right (e, s) -> Right (AST.BinaryExpression lhs (op _t) e, s)
exponExpr state@(t@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral}) : r@(_t@(Lexer.Token {Lexer.tokenType = Lexer.Power}) : ts)) lhs =
  case exponExpr ts (numExpr t) of
    Left pe -> Left pe
    Right (e, s) -> Right (AST.BinaryExpression lhs (op _t) e, s)
exponExpr state@(t : r@(_t : ts)) lhs
  | tt t == Lexer.Identifier = Right (idResExpr, r)
  | tt t == Lexer.NumericLiteral = Right (numResExpr, r)
  | tt t == Lexer.OpenParen =
      case parenExpr r of
        Left pe -> Left pe
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (parenResExpr e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (parenResExpr e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) (parenResExpr e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) (parenResExpr e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Power}) : sts) ->
          case exponExpr sts e of
            Left pe -> Left pe
            Right (e, s) -> Right (AST.BinaryExpression lhs (op st) e, s)
        Right (e, s) -> Right (AST.BinaryExpression lhs (AST.Operator Primitives.sPower) e, s)
  | otherwise = invalidExponExpr t
  where
    parenResExpr = AST.BinaryExpression lhs (AST.Operator Primitives.sPower)
    idResExpr = AST.BinaryExpression lhs (AST.Operator Primitives.sPower) (idExpr t)
    numResExpr = AST.BinaryExpression lhs (AST.Operator Primitives.sPower) (numExpr t)

-- | Unary expression.
unarExpr :: ParserState.ParserState -> AST.TreeNode -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
unarExpr [] _ = Left Error.syntaxError
unarExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier})] _op = Right (AST.UnaryExpression _op (idExpr token), [])
unarExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral})] _op = Right (AST.UnaryExpression _op (numExpr token), [])
unarExpr [t] op = invalidUnarExpr t
unarExpr state@(t : r@(_t : ts)) _op
  | tt t == Lexer.Identifier = Right (resExpr (idExpr t), r)
  | tt t == Lexer.NumericLiteral = Right (resExpr (idExpr t), r)
  | tt t == Lexer.OpenParen =
      case parenExpr r of
        Left pe -> Left pe
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (resExpr e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (resExpr e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) (resExpr e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) (resExpr e)
        Right (e, s) -> Right (resExpr e, s)
  | otherwise = invalidUnarExpr t
  where
    resExpr = AST.UnaryExpression _op

-- | Parenthesised expression.
parenExpr :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
parenExpr state =
  case expression state of
    Left pe -> Left pe
    Right (e, s) ->
      case closedParen s of
        Left pe -> Left pe
        Right tos -> Right (AST.Expression e, tos)

-- | Additive expression.
addExpr :: ParserState.ParserState -> AST.TreeNode -> AST.TreeNode -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
addExpr [] _ _ = Left Error.syntaxError
addExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier})] op lhs = Right (AST.BinaryExpression lhs op (idExpr token), [])
addExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral})] op lhs = Right (AST.BinaryExpression lhs op (numExpr token), [])
addExpr [t] op lhs = invalidAddExpr t
addExpr state@(t : r@(_t : ts)) binOp lhs
  | tt t == Lexer.Identifier && isAddOp _t = addExpr ts (op _t) (AST.BinaryExpression lhs binOp (idExpr t))
  | tt t == Lexer.Identifier && isMultOp _t = multExpr ts (op _t) (idExpr t) >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.Identifier && isExpOp _t =
      case exponExpr ts (idExpr t) of
        Left pe -> Left pe
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) e >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) e >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
        Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.NumericLiteral && isAddOp _t = addExpr ts (op _t) (AST.BinaryExpression lhs binOp (numExpr t))
  | tt t == Lexer.NumericLiteral && isMultOp _t = multExpr ts (op _t) (numExpr t) >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.NumericLiteral && isExpOp _t =
      case exponExpr ts (numExpr t) of
        Left pe -> Left pe
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) e >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) e >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
        Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.OpenParen =
      case parenExpr r of
        Left pe -> Left pe
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) e >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) e >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
        Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Power}) : sts) ->
          case exponExpr sts e of
            Left pe -> Left pe
            Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
            Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
            Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) e >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
            Right (e, st@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) e >>= \(e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
            Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
        Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.Identifier = Right (AST.BinaryExpression lhs binOp (idExpr t), r)
  | tt t == Lexer.NumericLiteral = Right (AST.BinaryExpression lhs binOp (numExpr t), r)
  | otherwise = invalidAddExpr t

-- | Multiplicative expression.
multExpr :: ParserState.ParserState -> AST.TreeNode -> AST.TreeNode -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
multExpr [] _ _ = Left Error.syntaxError
multExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.Identifier})] op lhs = Right (AST.BinaryExpression lhs op (idExpr token), [])
multExpr [token@(Lexer.Token {Lexer.tokenType = Lexer.NumericLiteral})] op lhs = Right (AST.BinaryExpression lhs op (numExpr token), [])
multExpr [t] op lhs = invalidMultExpr t
multExpr state@(t : r@(_t : ts)) binOp lhs
  | tt t == Lexer.Identifier && isMultOp _t = multExpr ts (op _t) (AST.BinaryExpression lhs binOp (idExpr t))
  | tt t == Lexer.Identifier && isAddOp _t = addExpr ts (op _t) (AST.BinaryExpression lhs binOp (idExpr t))
  | tt t == Lexer.Identifier && isExpOp _t =
      case exponExpr ts (idExpr t) of
        Left pe -> Left pe
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.NumericLiteral && isMultOp _t = multExpr ts (op _t) (AST.BinaryExpression lhs binOp (numExpr t))
  | tt t == Lexer.NumericLiteral && isAddOp _t = addExpr ts (op _t) (AST.BinaryExpression lhs binOp (numExpr t))
  | tt t == Lexer.NumericLiteral && isExpOp _t =
      case exponExpr ts (numExpr t) of
        Left pe -> Left pe
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.OpenParen =
      case parenExpr r of
        Left pe -> Left pe
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) (AST.BinaryExpression lhs binOp e)
        Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Power}) : sts) ->
          case exponExpr sts e of
            Left pe -> Left pe
            Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Plus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
            Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Minus}) : sts) -> addExpr sts (op st) (AST.BinaryExpression lhs binOp e)
            Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Multiply}) : sts) -> multExpr sts (op st) (AST.BinaryExpression lhs binOp e)
            Right (e, st@token@(Lexer.Token {Lexer.tokenType = Lexer.Divide}) : sts) -> multExpr sts (op st) (AST.BinaryExpression lhs binOp e)
            Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
        Right (e, s) -> Right (AST.BinaryExpression lhs binOp e, s)
  | tt t == Lexer.Identifier = Right (AST.BinaryExpression lhs binOp (idExpr t), r)
  | tt t == Lexer.NumericLiteral = Right (AST.BinaryExpression lhs binOp (numExpr t), r)
  | otherwise = invalidMultExpr t

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

isExpOp :: Lexer.Token -> Bool
isExpOp t = tt t == Lexer.Power

invalidExpr :: Lexer.Token -> Either Error.ParserException b
invalidExpr token =
  Left
    ( Error.ParserException
        { Error.pexMessage = "Invalid expression '" ++ Lexer.tokenValue token ++ "'.",
          Error.pexErrCode = Error.errInvalidExpression,
          Error.pexLineNum = Just (Lexer.tokenLineNum token),
          Error.pexColNum = Just (Lexer.tokenColumn token),
          Error.pexFileName = Lexer.fileName token
        }
    )

invalidMultExpr :: Lexer.Token -> Either Error.ParserException b
invalidMultExpr token =
  Left
    ( Error.ParserException
        { Error.pexMessage = "Invalid multiplicative expression '" ++ Lexer.tokenValue token ++ "'.",
          Error.pexErrCode = Error.errInvalidExpression,
          Error.pexLineNum = Just (Lexer.tokenLineNum token),
          Error.pexColNum = Just (Lexer.tokenColumn token),
          Error.pexFileName = Lexer.fileName token
        }
    )

invalidAddExpr :: Lexer.Token -> Either Error.ParserException b
invalidAddExpr token =
  Left
    ( Error.ParserException
        { Error.pexMessage = "Invalid additive expression '" ++ Lexer.tokenValue token ++ "'.",
          Error.pexErrCode = Error.errInvalidExpression,
          Error.pexLineNum = Just (Lexer.tokenLineNum token),
          Error.pexColNum = Just (Lexer.tokenColumn token),
          Error.pexFileName = Lexer.fileName token
        }
    )

invalidExponExpr :: Lexer.Token -> Either Error.ParserException b
invalidExponExpr token =
  Left
    ( Error.ParserException
        { Error.pexMessage = "Invalid exponantiation '" ++ Lexer.tokenValue token ++ "'.",
          Error.pexErrCode = Error.errInvalidExpression,
          Error.pexLineNum = Just (Lexer.tokenLineNum token),
          Error.pexColNum = Just (Lexer.tokenColumn token),
          Error.pexFileName = Lexer.fileName token
        }
    )

invalidUnarExpr :: Lexer.Token -> Either Error.ParserException b
invalidUnarExpr token =
  Left
    ( Error.ParserException
        { Error.pexMessage = "Invalid unary expression '" ++ Lexer.tokenValue token ++ "'.",
          Error.pexErrCode = Error.errInvalidExpression,
          Error.pexLineNum = Just (Lexer.tokenLineNum token),
          Error.pexColNum = Just (Lexer.tokenColumn token),
          Error.pexFileName = Lexer.fileName token
        }
    )