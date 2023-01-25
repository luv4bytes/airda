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