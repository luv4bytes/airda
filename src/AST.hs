module AST where

-- | Defines a node in a parse tree.
data TreeNode
  = -- | Defines the root node of a parsing tree.
    Root
      { nodes :: [TreeNode],
        fileName :: String
      }
  | -- | Defines a module declaration node.
    ModuleDecl
      { modId :: TreeNode
      }
  | -- | Defines a variable declaration node in the parse tree.
    VariableDecl
      { ident :: TreeNode,
        typeIdent :: TreeNode
      }
  | -- | Defines a variable declaration with initialization.
    VariableInit
      { ident :: TreeNode,
        typeIdent :: TreeNode,
        initValue :: TreeNode
      }
  | -- | Defines an assignment for a variable.
    VariableAssignment
      { variableName :: TreeNode,
        assignValue :: TreeNode
      }
  | -- | Defines a numeric literal node.
    NumericLiteral
      { value :: String
      }
  | -- | Defines a type identifier node.
    TypeIdentifier
      { value :: String
      }
  | -- | Defines an identifier node.
    Identifier
      { value :: String
      }
  | UnaryOperator
      { value :: String
      }
  | UnaryExpression
      { operator :: TreeNode,
        expression :: TreeNode
      }
  deriving (Show, Eq)

-- | Defines as a list of tree nodes.
type NodeList = [TreeNode]