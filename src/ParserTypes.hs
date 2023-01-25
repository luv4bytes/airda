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

-- | ParserTypes contains types used during parsing.
module ParserTypes where

import qualified LexerTypes as LC
import qualified LexerTypes as LT

-- | Defines a node in a parse tree.
data TreeNode
  = EmptyNode
  | -- | Defines the root node of a parsing tree.
    TreeRoot
      { nodes :: [TreeNode],
        fileName :: String
      }
  | -- | Defines a module declaration node.
    ModuleDeclNode
      { modId :: TreeNode
      }
  | -- | Defines a variable declaration node in the parse tree.
    VariableDeclNode
      { ident :: TreeNode,
        typeIdent :: TreeNode
      }
  | -- | Defines a variable declaration with initialization.
    VariableInitNode
      { ident :: TreeNode,
        typeIdent :: TreeNode,
        initValue :: TreeNode
      }
  | -- | Defines an assignment for a variable.
    VariableAssignmentNode
      { variableName :: TreeNode,
        assignValue :: TreeNode
      }
  | -- | Defines a numeric literal node.
    NumericLiteralNode
      { value :: String
      }
  | -- | Defines a type identifier node.
    TypeIdentifierNode
      { value :: String
      }
  | -- | Defines an identifier node.
    IdentifierNode
      { value :: String
      }
  deriving (Show, Eq)

-- | Defines as a list of tree nodes.
type NodeList = [TreeNode]

type ParserState = LT.TokenList