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
module Parser where

import qualified AST
import qualified Control.Monad
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Error
import qualified Expression
import qualified Lexer
import qualified ParserState

-- | Defines the parser state.
type ParserState = Lexer.TokenList

-- | Parses the given tokens into a list of tree nodes.
parseTokens :: Lexer.TokenList -> String -> Either Error.ParserException AST.TreeNode
parseTokens [] _ = Left (Error.ParserExceptionSimple "Nothing to parse :(")
parseTokens tokens fileName =
  case program tokens of
    Left pe -> Left pe
    Right prog -> Right (AST.Root {AST.nodes = prog, AST.fileName = fileName})
  where
    {-- The following functions represent parsing rules according to the Airda grammar. --}

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
    statements tokens@(t : tt : ts) nodes
      | Lexer.tokenType t == Lexer.EndOfStatement = statements ts nodes
      | Lexer.tokenType t == Lexer.Module =
          case moduleDeclaration tokens of
            Left ex -> Left ex
            Right (assignment, pstate) -> statements pstate (nodes ++ [assignment])
      | Lexer.tokenType t == Lexer.Identifier && Lexer.tokenType tt == Lexer.TypeSpecifier =
          case variableDeclaration tokens of
            Left ex -> Left ex
            Right (varDecl, pstate) -> statements pstate (nodes ++ [varDecl])
      | Lexer.tokenType t == Lexer.Identifier && Lexer.tokenType tt == Lexer.Assignment =
          case variableAssignment tokens of
            Left ex -> Left ex
            Right (assignment, pstate) -> statements pstate (nodes ++ [assignment])
      | otherwise =
          Left
            ( Error.ParserException
                ("Invalid token '" ++ Lexer.tokenValue t ++ "'.")
                Error.errInvalidToken
                (Just (Lexer.tokenLineNum t))
                (Just (Lexer.tokenColumn t))
                (Lexer.fileName t)
            )

    moduleDeclaration :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    moduleDeclaration [] = Left (Error.ParserExceptionSimple "Expected module declaration.")
    moduleDeclaration (t : ts)
      | Lexer.tokenType t == Lexer.Module =
          case identifier ts of
            Left pe -> Left pe
            Right (idNode, pstate) ->
              case endOfStatement pstate of
                Left pe -> Left pe
                Right tos ->
                  endOfStatement pstate >>= \pstate ->
                    Right (AST.ModuleDecl idNode, pstate)
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected module declaration.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    variableAssignment :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    variableAssignment [] = Left (Error.ParserExceptionSimple "Expected identifiError.")
    variableAssignment (t : ts)
      | Lexer.tokenType t == Lexer.Identifier =
          assignment ts
            >>= ( Expression.expression
                    Control.Monad.>=> ( \(exprNode, pstate) ->
                                          endOfStatement pstate
                                            >>= \pstate ->
                                              Right
                                                ( AST.Assignment
                                                    (AST.Identifier (Lexer.tokenValue t))
                                                    exprNode,
                                                  pstate
                                                )
                                      )
                )
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected identifiError.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    variableDeclaration :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    variableDeclaration [] = Left (Error.ParserExceptionSimple "Expected identifiError.")
    variableDeclaration (t : ts)
      | Lexer.tokenType t == Lexer.Identifier =
          case typeSpecifier ts of
            Left pe -> Left pe
            Right pstate ->
              case typeIdentifier pstate of
                Left pe -> Left pe
                Right (typeIdNode, pstate) ->
                  case assignment pstate of
                    Left pe ->
                      case endOfStatement pstate of
                        Left pe' -> Left pe'
                        Right pstate ->
                          Right (AST.VariableDecl idNode typeIdNode, pstate)
                    Right pstate ->
                      case Expression.expression pstate of
                        Left pe -> Left pe
                        Right (exprNode, pstate) ->
                          endOfStatement pstate >>= \pstate ->
                            Right (AST.VariableInit idNode typeIdNode exprNode, pstate)
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected identifiError.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )
      where
        idNode = AST.Identifier (Lexer.tokenValue t)

    typeSpecifier :: ParserState.ParserState -> Either Error.ParserException ParserState.ParserState
    typeSpecifier [] = Left (Error.ParserExceptionSimple "Expected type specifiError.")
    typeSpecifier (t : ts)
      | Lexer.tokenType t == Lexer.TypeSpecifier = Right ts
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected type specifiError.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    identifier :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    identifier [] = Left (Error.ParserExceptionSimple "Expected identifiError.")
    identifier (t : ts)
      | Lexer.tokenType t == Lexer.Identifier = Right (AST.Identifier (Lexer.tokenValue t), ts)
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected identifiError.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    typeIdentifier :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
    typeIdentifier [] = Left (Error.ParserExceptionSimple "Expected type identifiError.")
    typeIdentifier (t : ts)
      | Lexer.tokenType t == Lexer.Identifier = Right (AST.TypeIdentifier (Lexer.tokenValue t), ts)
      | otherwise =
          Left
            ( Error.ParserException
                { Error.pexMessage = "Invalid token '" ++ Lexer.tokenValue t ++ "'. Expected type identifiError.",
                  Error.pexErrCode = Error.errInvalidToken,
                  Error.pexLineNum = Just (Lexer.tokenLineNum t),
                  Error.pexColNum = Just (Lexer.tokenColumn t),
                  Error.pexFileName = Lexer.fileName t
                }
            )

    assignment :: ParserState.ParserState -> Either Error.ParserException ParserState.ParserState
    assignment [] = Left (Error.ParserExceptionSimple "Expected end of statement or initialization.")
    assignment (t : ts)
      | Lexer.tokenType t == Lexer.Assignment = Right ts
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

    endOfStatement :: ParserState.ParserState -> Either Error.ParserException ParserState.ParserState
    endOfStatement [] = Left (Error.ParserExceptionSimple "Expected end of statement.")
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

-- | Returns a string representation of a parse AST.
treeRepr :: AST.TreeNode -> String
treeRepr (AST.Root nodes fileName) =
  "[" ++ fileName ++ "]\n" ++ treeRepr' nodes 2 ++ "\n"
  where
    treeRepr' :: AST.NodeList -> Int -> String
    treeRepr' [] _ = ""
    treeRepr' (x : xs) level = treeRepr'' x level ++ treeRepr' xs level
      where
        treeRepr'' :: AST.TreeNode -> Int -> String
        treeRepr'' (AST.ModuleDecl id) level =
          replicate level '•'
            ++ "Module declaration\n"
            ++ treeRepr'' id (level + 2)
        treeRepr'' (AST.Identifier value) level =
          replicate level ' '
            ++ "Id: "
            ++ value
            ++ "\n"
        treeRepr'' (AST.TypeIdentifier value) level =
          replicate level ' '
            ++ "Type Id: "
            ++ value
            ++ "\n"
        treeRepr'' (AST.UnaryExpression op expr) level =
          replicate level ' '
            ++ "Unary expression\n"
            ++ treeRepr'' op (level + 2)
            ++ treeRepr'' expr (level + 2)
        treeRepr'' (AST.UnaryOperator value) level =
          replicate level ' '
            ++ "Operator: "
            ++ value
            ++ "\n"
        treeRepr'' (AST.NumericLiteral value) level =
          replicate level ' '
            ++ "Numeric literal: "
            ++ value
            ++ "\n"
        treeRepr'' (AST.VariableDecl id typeId) level =
          replicate level '•'
            ++ "Variable declaration\n"
            ++ treeRepr'' id (level + 2)
            ++ treeRepr'' typeId (level + 2)
        treeRepr'' (AST.VariableInit id typeId expr) level =
          replicate level '•'
            ++ "Variable initialization\n"
            ++ treeRepr'' id (level + 2)
            ++ treeRepr'' typeId (level + 2)
            ++ treeRepr'' expr (level + 2)
        treeRepr'' (AST.Assignment id expr) level =
          replicate level '•'
            ++ "Assignment\n"
            ++ treeRepr'' id (level + 2)
            ++ treeRepr'' expr (level + 2)
        treeRepr'' _ _ = ""
treeRepr _ = ""