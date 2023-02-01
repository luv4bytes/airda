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
import qualified Control.Monad
import qualified Error
import qualified Lexer
import qualified ParserState

-- | Parses the given tokens into a list of tree nodes.
parse :: Lexer.TokenList -> String -> Either Error.ParserException AST.TreeNode
parse tokens fileName = Left (Error.ParserExceptionSimple "Nothing to parse :(")

expression :: ParserState.ParserState -> Either Error.ParserException (AST.TreeNode, ParserState.ParserState)
expression [] = Left (Error.ParserExceptionSimple "Expected expression.")
expression state@(t : ts)
  | Lexer.tokenType t == Lexer.Identifier = Right (AST.Expression (AST.Identifier (tv t)), ts)
  | Lexer.tokenType t == Lexer.NumericLiteral = Right (AST.Expression (AST.Identifier (tv t)), ts)
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
    tv = Lexer.tokenValue

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
        treeRepr'' (AST.Operator value prec) level =
          "Op: "
            ++ value
            ++ " ("
            ++ show prec
            ++ ")"
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