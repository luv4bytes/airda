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

-- | Parser contains functions to parse tokens into a parse tree.
module Parser where

import qualified Control.Monad
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Error as ER
import qualified Expression as E
import qualified LexerTypes as LT
import qualified ParserTypes as PT

-- | Parses the given tokens into a list of tree nodes.
parseTokens :: LT.TokenList -> String -> Either ER.ParserException PT.TreeNode
parseTokens [] _ = Left (ER.ParserExceptionSimple "Nothing to parse :(")
parseTokens tokens fileName =
  case program tokens of
    Left pe -> Left pe
    Right prog -> Right (PT.TreeRoot {PT.nodes = prog, PT.fileName = fileName})
  where
    {-- The following functions represent parsing rules according to the Airda grammar. --}

    program :: PT.ParserState -> Either ER.ParserException PT.NodeList
    program tokens = statements tokens []

    statements :: PT.ParserState -> PT.NodeList -> Either ER.ParserException PT.NodeList
    statements [] nodes = Right nodes
    statements [t] nodes
      | LT.tokenType t == LT.EndOfStatement = Right nodes
      | otherwise =
          Left
            ( ER.ParserException
                { ER.pexMessage = "Invalid token '" ++ LT.tokenValue t ++ "'. Expected end of statement.",
                  ER.pexErrCode = ER.errInvalidToken,
                  ER.pexLineNum = Just (LT.tokenLineNum t),
                  ER.pexColNum = Just (LT.tokenColumn t),
                  ER.pexFileName = LT.fileName t
                }
            )
    statements tokens@(t : tt : ts) nodes
      | LT.tokenType t == LT.EndOfStatement = statements ts nodes
      | LT.tokenType t == LT.Module =
          case moduleDeclaration tokens of
            Left ex -> Left ex
            Right (assignment, pstate) -> statements pstate (nodes ++ [assignment])
      | LT.tokenType t == LT.Identifier && LT.tokenType tt == LT.TypeSpecifier =
          case variableDeclaration tokens of
            Left ex -> Left ex
            Right (varDecl, pstate) -> statements pstate (nodes ++ [varDecl])
      | LT.tokenType t == LT.Identifier && LT.tokenType tt == LT.Assignment =
          case variableAssignment tokens of
            Left ex -> Left ex
            Right (assignment, pstate) -> statements pstate (nodes ++ [assignment])
      | otherwise =
          Left
            ( ER.ParserException
                ("Invalid token '" ++ LT.tokenValue t ++ "'.")
                ER.errInvalidToken
                (Just (LT.tokenLineNum t))
                (Just (LT.tokenColumn t))
                (LT.fileName t)
            )

    moduleDeclaration :: PT.ParserState -> Either ER.ParserException (PT.TreeNode, PT.ParserState)
    moduleDeclaration [] = Left (ER.ParserExceptionSimple "Expected module declaration.")
    moduleDeclaration (t : ts)
      | LT.tokenType t == LT.Module =
          case identifier ts of
            Left pe -> Left pe
            Right (idNode, pstate) ->
              case endOfStatement pstate of
                Left pe -> Left pe
                Right tos ->
                  endOfStatement pstate >>= \pstate ->
                    Right (PT.ModuleDeclNode idNode, pstate)
      | otherwise =
          Left
            ( ER.ParserException
                { ER.pexMessage = "Invalid token '" ++ LT.tokenValue t ++ "'. Expected module declaration.",
                  ER.pexErrCode = ER.errInvalidToken,
                  ER.pexLineNum = Just (LT.tokenLineNum t),
                  ER.pexColNum = Just (LT.tokenColumn t),
                  ER.pexFileName = LT.fileName t
                }
            )

    variableAssignment :: PT.ParserState -> Either ER.ParserException (PT.TreeNode, PT.ParserState)
    variableAssignment [] = Left (ER.ParserExceptionSimple "Expected identifier.")
    variableAssignment (t : ts)
      | LT.tokenType t == LT.Identifier =
          assignment ts
            >>= ( E.expression
                    Control.Monad.>=> ( \(exprNode, pstate) ->
                                          endOfStatement pstate
                                            >>= \pstate ->
                                              Right
                                                ( PT.VariableAssignmentNode
                                                    (PT.IdentifierNode (LT.tokenValue t))
                                                    exprNode,
                                                  pstate
                                                )
                                      )
                )
      | otherwise =
          Left
            ( ER.ParserException
                { ER.pexMessage = "Invalid token '" ++ LT.tokenValue t ++ "'. Expected identifier.",
                  ER.pexErrCode = ER.errInvalidToken,
                  ER.pexLineNum = Just (LT.tokenLineNum t),
                  ER.pexColNum = Just (LT.tokenColumn t),
                  ER.pexFileName = LT.fileName t
                }
            )

    variableDeclaration :: PT.ParserState -> Either ER.ParserException (PT.TreeNode, PT.ParserState)
    variableDeclaration [] = Left (ER.ParserExceptionSimple "Expected identifier.")
    variableDeclaration (t : ts)
      | LT.tokenType t == LT.Identifier =
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
                          Right (PT.VariableDeclNode idNode typeIdNode, pstate)
                    Right pstate ->
                      case E.expression pstate of
                        Left pe -> Left pe
                        Right (exprNode, pstate) ->
                          endOfStatement pstate >>= \pstate ->
                            Right (PT.VariableInitNode idNode typeIdNode exprNode, pstate)
      | otherwise =
          Left
            ( ER.ParserException
                { ER.pexMessage = "Invalid token '" ++ LT.tokenValue t ++ "'. Expected identifier.",
                  ER.pexErrCode = ER.errInvalidToken,
                  ER.pexLineNum = Just (LT.tokenLineNum t),
                  ER.pexColNum = Just (LT.tokenColumn t),
                  ER.pexFileName = LT.fileName t
                }
            )
      where
        idNode = PT.IdentifierNode (LT.tokenValue t)

    typeSpecifier :: PT.ParserState -> Either ER.ParserException PT.ParserState
    typeSpecifier [] = Left (ER.ParserExceptionSimple "Expected type specifier.")
    typeSpecifier (t : ts)
      | LT.tokenType t == LT.TypeSpecifier = Right ts
      | otherwise =
          Left
            ( ER.ParserException
                { ER.pexMessage = "Invalid token '" ++ LT.tokenValue t ++ "'. Expected type specifier.",
                  ER.pexErrCode = ER.errInvalidToken,
                  ER.pexLineNum = Just (LT.tokenLineNum t),
                  ER.pexColNum = Just (LT.tokenColumn t),
                  ER.pexFileName = LT.fileName t
                }
            )

    identifier :: PT.ParserState -> Either ER.ParserException (PT.TreeNode, PT.ParserState)
    identifier [] = Left (ER.ParserExceptionSimple "Expected identifier.")
    identifier (t : ts)
      | LT.tokenType t == LT.Identifier = Right (PT.IdentifierNode (LT.tokenValue t), ts)
      | otherwise =
          Left
            ( ER.ParserException
                { ER.pexMessage = "Invalid token '" ++ LT.tokenValue t ++ "'. Expected identifier.",
                  ER.pexErrCode = ER.errInvalidToken,
                  ER.pexLineNum = Just (LT.tokenLineNum t),
                  ER.pexColNum = Just (LT.tokenColumn t),
                  ER.pexFileName = LT.fileName t
                }
            )

    typeIdentifier :: PT.ParserState -> Either ER.ParserException (PT.TreeNode, PT.ParserState)
    typeIdentifier [] = Left (ER.ParserExceptionSimple "Expected type identifier.")
    typeIdentifier (t : ts)
      | LT.tokenType t == LT.Identifier = Right (PT.TypeIdentifierNode (LT.tokenValue t), ts)
      | otherwise =
          Left
            ( ER.ParserException
                { ER.pexMessage = "Invalid token '" ++ LT.tokenValue t ++ "'. Expected type identifier.",
                  ER.pexErrCode = ER.errInvalidToken,
                  ER.pexLineNum = Just (LT.tokenLineNum t),
                  ER.pexColNum = Just (LT.tokenColumn t),
                  ER.pexFileName = LT.fileName t
                }
            )

    assignment :: PT.ParserState -> Either ER.ParserException PT.ParserState
    assignment [] = Left (ER.ParserExceptionSimple "Expected end of statement or initialization.")
    assignment (t : ts)
      | LT.tokenType t == LT.Assignment = Right ts
      | otherwise =
          Left
            ( ER.ParserException
                { ER.pexMessage = "Invalid token '" ++ LT.tokenValue t ++ "'. Expected end of statement.",
                  ER.pexErrCode = ER.errInvalidToken,
                  ER.pexLineNum = Just (LT.tokenLineNum t),
                  ER.pexColNum = Just (LT.tokenColumn t),
                  ER.pexFileName = LT.fileName t
                }
            )

    endOfStatement :: PT.ParserState -> Either ER.ParserException PT.ParserState
    endOfStatement [] = Left (ER.ParserExceptionSimple "Expected end of statement.")
    endOfStatement (t : ts)
      | LT.tokenType t == LT.EndOfStatement = Right ts
      | otherwise =
          Left
            ( ER.ParserException
                { ER.pexMessage = "Invalid token '" ++ LT.tokenValue t ++ "'. Expected end of statement.",
                  ER.pexErrCode = ER.errInvalidToken,
                  ER.pexLineNum = Just (LT.tokenLineNum t),
                  ER.pexColNum = Just (LT.tokenColumn t),
                  ER.pexFileName = LT.fileName t
                }
            )

-- | Returns a string representation of a parse tree.
treeRepr :: PT.TreeNode -> String
treeRepr (PT.TreeRoot nodes fileName) =
  "[" ++ fileName ++ "]\n" ++ treeRepr' nodes 2 ++ "\n"
  where
    treeRepr' :: PT.NodeList -> Int -> String
    treeRepr' [] _ = ""
    treeRepr' (x : xs) level = treeRepr'' x level ++ treeRepr' xs level
      where
        treeRepr'' :: PT.TreeNode -> Int -> String
        treeRepr'' (PT.ModuleDeclNode id) level =
          replicate level '•'
            ++ "Module declaration\n"
            ++ treeRepr'' id (level + 2)
        treeRepr'' (PT.IdentifierNode value) level =
          replicate level ' '
            ++ "Id: "
            ++ value
            ++ "\n"
        treeRepr'' (PT.TypeIdentifierNode value) level =
          replicate level ' '
            ++ "Type Id: "
            ++ value
            ++ "\n"
        treeRepr'' (PT.NumericLiteralNode value) level =
          replicate level ' '
            ++ "Numeric literal: "
            ++ value
            ++ "\n"
        treeRepr'' (PT.VariableDeclNode id typeId) level =
          replicate level '•'
            ++ "Variable declaration\n"
            ++ treeRepr'' id (level + 2)
            ++ treeRepr'' typeId (level + 2)
        treeRepr'' (PT.VariableInitNode id typeId expr) level =
          replicate level '•'
            ++ "Variable initialization\n"
            ++ treeRepr'' id (level + 2)
            ++ treeRepr'' typeId (level + 2)
            ++ replicate (level + 2) ' '
            ++ "Expr\n"
            ++ treeRepr'' expr (level + 4)
        treeRepr'' (PT.VariableAssignmentNode id expr) level =
          replicate level '•'
            ++ "Variable assignment\n"
            ++ treeRepr'' id (level + 2)
            ++ replicate (level + 2) ' '
            ++ "Expr\n"
            ++ treeRepr'' expr (level + 4)
        treeRepr'' _ _ = ""
treeRepr _ = ""