-- | LexerTypes contains types used during lexical analysis.
module LexerTypes where

-- | Defines different types of tokens found during lexical analysis.
data TokenType
  = -- | Identifier such as variables, functions, types etc.
    Identifier
  | -- | Module declaration keyword.
    Module
  | -- | Type specifier token.
    TypeSpecifier
  | -- | Assignment operator.
    Assignment
  | -- | Numeric value.
    Numeric
  | -- | Minus sign.
    Minus
  | -- | End of statement token.
    EndOfStatement
  | -- | Unknown token.
    Unknown
  deriving (Show, Eq)

-- | Defines a token found during lexical analysis.
data Token = Token
  { tokenType :: TokenType,
    tokenValue :: String,
    tokenLineNum :: Int,
    tokenColumn :: Int,
    fileName :: String
  }
  deriving (Show, Eq)

-- | List of tokens.
type TokenList = [Token]