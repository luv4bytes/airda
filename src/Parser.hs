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

-- | Parser contains functions to parse tokens into a parse AST.
module Parser (parse, treeRepr) where

import qualified AST
import Data.Maybe (fromMaybe)
import qualified Error
import qualified Lexer
import qualified ParserState

-- | Operator associativity.
data OpAssoc = L | R

-- | Operator associativities.
opAssocs :: [(Lexer.TokenType, OpAssoc)]
opAssocs =
  [ (Lexer.Plus, L),
    (Lexer.Minus, L),
    (Lexer.Multiply, L),
    (Lexer.Divide, L)
  ]

-- | Returns the associativity value for the given token. If it is not found L is assumed.
opassoc :: Lexer.Token -> OpAssoc
opassoc t = fromMaybe L (lookup (Lexer.tokenType t) opAssocs)

-- | Operator precedences.
opPrecs :: [(Lexer.TokenType, Integer)]
opPrecs =
  [ (Lexer.Plus, 1),
    (Lexer.Minus, 1),
    (Lexer.Multiply, 2),
    (Lexer.Divide, 2)
  ]

-- | Returns the operator precedence for the given token. If it is not found 1 is assumed.
opprec :: Lexer.Token -> Integer
opprec t = fromMaybe 1 (lookup (Lexer.tokenType t) opPrecs)

-- | Parses the given tokens into a list of tree nodes.
parse :: Lexer.TokenList -> String -> Either Error.ParserException AST.TreeNode
parse [] _ = Left (Error.ParserExceptionSimple "Nothing to parse :(")
parse tokens fileName = program tokens >>= \nodes -> Right (AST.Root nodes fileName)
  where
    program :: ParserState.ParserState -> Either Error.ParserException AST.NodeList
    program tokens = statements tokens []

    statements :: ParserState.ParserState -> AST.NodeList -> Either Error.ParserException AST.NodeList
    statements [] nodes = Right nodes
    statements [t] nodes
      | Lexer.tokenType t == Lexer.EndOfStatement = Right nodes
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected end of statement.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )
    statements tokens@(t : rest@(tt : ts)) nodes
      | Lexer.tokenType t == Lexer.EndOfStatement = statements rest nodes
      | Lexer.tokenType t == Lexer.Identifier && Lexer.tokenType tt == Lexer.TypeSpecifier =
          case vardecl tokens of
            Left pe -> Left pe
            Right (vdNode, state) -> statements state (nodes ++ [vdNode])
      | Lexer.tokenType t == Lexer.Identifier && Lexer.tokenType tt == Lexer.AssignOp =
          case assign tokens of
            Left pe -> Left pe
            Right (aNode, state) -> statements state (nodes ++ [aNode])
      | otherwise =
          Left
            ( Error.ParserException
                ("Invalid statement '" ++ Lexer.tokenValue t ++ "'.")
                Error.errInvalidToken
                (Just (Lexer.tokenLineNum t))
                (Just (Lexer.tokenColumn t))
                (Lexer.fileName t)
            )

    vardecl :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    vardecl [] = Left (Error.ParserExceptionSimple "Syntax error.")
    vardecl state =
      case identifier state of
        Left pe -> Left pe
        Right (idNode, state) ->
          case typeSpecifier state of
            Left pe -> Left pe
            Right state ->
              case typeIdentifier state of
                Left pe -> Left pe
                Right (typeId, state) ->
                  case endOfStatement state of
                    Left pe -> Left pe
                    Right state -> Right (AST.VarDecl idNode typeId, state)

    assign :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    assign state =
      case identifier state of
        Left pe -> Left pe
        Right (idNode, state) ->
          case assignOp state of
            Left pe -> Left pe
            Right state ->
              case expression state of
                Left pe -> Left pe
                Right (expr, state) ->
                  case endOfStatement state of
                    Left pe -> Left pe
                    Right state -> Right (AST.Assignment idNode expr, state)

    identifier :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    identifier [] = Left (Error.ParserExceptionSimple "Syntax error.")
    identifier (t : ts)
      | Lexer.tokenType t == Lexer.Identifier = Right (AST.Identifier (Lexer.tokenValue t), ts)
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected identifier.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    typeIdentifier :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    typeIdentifier [] = Left (Error.ParserExceptionSimple "Syntax error.")
    typeIdentifier (t : ts)
      | Lexer.tokenType t == Lexer.Identifier = Right (AST.TypeIdentifier (Lexer.tokenValue t), ts)
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected type identifier.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    typeSpecifier :: ParserState.ParserState -> Either Error.ParserException ParserState.ParserState
    typeSpecifier [] = Left (Error.ParserExceptionSimple "Syntax error.")
    typeSpecifier (t : ts)
      | Lexer.tokenType t == Lexer.TypeSpecifier = Right ts
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected type specifier.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    assignOp :: ParserState.ParserState -> Either Error.ParserException ParserState.ParserState
    assignOp [] = Left (Error.ParserExceptionSimple "Syntax error.")
    assignOp (t : ts)
      | Lexer.tokenType t == Lexer.AssignOp = Right ts
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected assignment operator.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    endOfStatement :: ParserState.ParserState -> Either Error.ParserException ParserState.ParserState
    endOfStatement [] = Left (Error.ParserExceptionSimple "Syntax error.")
    endOfStatement (t : ts)
      | Lexer.tokenType t == Lexer.EndOfStatement = Right ts
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected end of statement.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    expression :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    expression [] = Left (Error.ParserExceptionSimple "Syntax error.")
    expression [t]
      | tt t == Lexer.Identifier = Right (idexpr, [])
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
        idexpr = AST.Expression (AST.Identifier (tv t))
        tv = Lexer.tokenValue
        tt = Lexer.tokenType
    expression state@(t : r@(_t : ts))
      | tt t == Lexer.Identifier && tt _t == Lexer.Plus = addExpr ts plusOp idexpr
      | tt t == Lexer.Identifier = Right (idexpr, r)
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
        idexpr = AST.Expression (AST.Identifier (tv t))
        plusOp = AST.Operator (tv _t)

        tv = Lexer.tokenValue
        tt = Lexer.tokenType

        -- \| Additive Expression
        addExpr :: ParserState.ParserState -> AST.TreeNode -> AST.TreeNode -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
        addExpr [] _ _ = Left (Error.ParserExceptionSimple "Syntax error.")
        addExpr [t] op lhs
          | tt t == Lexer.Identifier = Right (AST.BinaryExpression lhs op idexpr, [])
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
            idexpr = AST.Expression (AST.Identifier (tv t))
        addExpr state@(t : r@(_t : ts)) op lhs
          | tt t == Lexer.Identifier && tt _t == Lexer.Plus =
              case addExpr ts plusOp (AST.BinaryExpression lhs op idexpr) of
                Left pe -> Left pe
                Right (e, s) -> Right (e, s)
          | tt t == Lexer.Identifier = Right (AST.BinaryExpression lhs op idexpr, r)
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
            idexpr = AST.Expression (AST.Identifier (tv t))
            plusOp = AST.Operator (tv _t)

-- | Returns a string representation of a parse AST.
treeRepr :: AST.TreeNode -> String
treeRepr (AST.Root nodes fileName) =
  "[" ++ fileName ++ "]\n" ++ treeRepr' nodes 1 ++ "\n"
  where
    treeRepr' :: AST.NodeList -> Int -> String
    treeRepr' [] _ = ""
    treeRepr' (x : xs) level = treeRepr'' x level ++ treeRepr' xs level
      where
        treeRepr'' :: AST.TreeNode -> Int -> String
        treeRepr'' (AST.ModuleDecl id) level =
          replicate level '└'
            ++ "Mod\n"
            ++ replicate level ' '
            ++ "└─"
            ++ treeRepr'' id level
        treeRepr'' (AST.Assignment id expr) level =
          replicate level '└'
            ++ "Assng\n"
            ++ replicate level ' '
            ++ "└─"
            ++ treeRepr'' id level
            ++ replicate level ' '
            ++ "└─"
            ++ treeRepr'' expr level
        treeRepr'' (AST.VarDecl id typeId) level =
          replicate level '└'
            ++ "VarDecl\n"
            ++ replicate level ' '
            ++ "└─"
            ++ treeRepr'' id level
            ++ replicate level ' '
            ++ "└─"
            ++ treeRepr'' typeId level
        treeRepr'' (AST.VarInit id typeId expr) level =
          replicate level '└'
            ++ "VarInit\n"
            ++ replicate (level + 2) ' '
            ++ "└─"
            ++ treeRepr'' id (level + 2)
            ++ replicate (level + 2) ' '
            ++ "└─"
            ++ treeRepr'' typeId (level + 2)
            ++ replicate (level + 2) ' '
            ++ "└─"
            ++ treeRepr'' expr (level + 2)
        treeRepr'' (AST.Identifier value) level =
          "Id: "
            ++ value
            ++ "\n"
        treeRepr'' (AST.NumericLiteral value) level =
          "Num: "
            ++ value
            ++ "\n"
        treeRepr'' (AST.TypeIdentifier value) level =
          "T_Id: "
            ++ value
            ++ "\n"
        treeRepr'' (AST.Operator value) level =
          "Op: "
            ++ value
            ++ "\n"
        treeRepr'' (AST.Expression expr) level =
          "Expr\n"
            ++ replicate (level + 2) ' '
            ++ "└─"
            ++ treeRepr'' expr (level + 2)
        treeRepr'' (AST.UnaryExpression op expr) level =
          "UnExpr\n"
            ++ replicate (level + 2) ' '
            ++ "└─"
            ++ treeRepr'' op (level + 2)
            ++ replicate (level + 2) ' '
            ++ "└─"
            ++ treeRepr'' expr (level + 2)
        treeRepr'' (AST.BinaryExpression lhs op rhs) level =
          "BinExpr\n"
            ++ replicate (level + 2) ' '
            ++ "└─"
            ++ treeRepr'' lhs (level + 2)
            ++ replicate (level + 2) ' '
            ++ "└─"
            ++ treeRepr'' op (level + 2)
            ++ replicate (level + 2) ' '
            ++ "└─"
            ++ treeRepr'' rhs (level + 2)
        treeRepr'' _ _ = ""
treeRepr _ = ""