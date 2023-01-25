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