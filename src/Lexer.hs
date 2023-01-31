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

-- | Lexer contains functions to perform lexical analysis of sources.
module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Keywords
import qualified Operators

-- | Defines different types of tokens found during lexical analysis.
data TokenType
  = -- | Identifier such as variables, functions, types etc.
    Identifier
  | -- | Module declaration keyword.
    Module
  | -- | Type specifier token.
    TypeSpecifier
  | -- | Assignment operator.
    AssignOp
  | -- | Numeric value.
    NumericLiteral
  | -- | Minus sign.
    Minus
  | -- | Plus sign.
    Plus
  | -- | Divide sign.
    Divide
  | -- | Multiply sign.
    Multiply
  | -- | End of statement token.
    EndOfStatement
  | -- | Open parenthesis.
    OpenParen
  | -- | Closed parenthesis.
    ClosedParen
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

-- | Splits the given line into a list of tokens.
tokenizeLine :: String -> String -> Int -> TokenList
tokenizeLine fileName line lineNum = tokenizeLine' lineNum 1 [] line []
  where
    tokenizeLine' :: Int -> Int -> [Char] -> String -> TokenList -> TokenList
    tokenizeLine' lineNum colNum stack [] tokens = tokens
    tokenizeLine' lineNum colNum stack (x : xs) tokens
      | isAlpha x =
          getIdentifier (stack ++ [x]) lineNum colNum xs tokens
      | isDigit x = getNumeric (stack ++ [x]) lineNum colNum xs tokens
      | x == '#' = tokenizeLine' lineNum (colNum + 1) stack [] tokens -- Comment. Whole line is not analyzed.
      | x == Operators.minus = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [Token Minus [x] lineNum colNum fileName])
      | x == Operators.plus = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [Token Plus [x] lineNum colNum fileName])
      | x == Operators.divide = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [Token Divide [x] lineNum colNum fileName])
      | x == Operators.multiply = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [Token Multiply [x] lineNum colNum fileName])
      | x == Keywords.assignment =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [Token AssignOp [x] lineNum colNum fileName])
      | x == Keywords.typeSpecifier =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [Token TypeSpecifier [x] lineNum (colNum - length [x] + 1) fileName])
      | x == Keywords.endOfStatement =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [Token EndOfStatement [x] lineNum colNum fileName])
      | x == Keywords.openParen =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [Token OpenParen [x] lineNum colNum fileName])
      | x == Keywords.closedParen =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [Token ClosedParen [x] lineNum colNum fileName])
      | isSpace x = tokenizeLine' lineNum (colNum + 1) stack xs tokens
      | otherwise = tokenizeLine' lineNum (colNum + 1) stack xs (tokens ++ [Token Unknown [x] lineNum colNum fileName])
      where
        -- \| Extracts the next identifier from the given line.
        getIdentifier :: [Char] -> Int -> Int -> String -> TokenList -> TokenList
        getIdentifier stack lineNum colNum [] tokens
          | stack == Keywords.moduleDecl = tokens ++ [Token Module stack lineNum (colNum - length stack + 1) fileName]
          | otherwise = tokens ++ [Token Identifier stack lineNum (colNum - length stack + 1) fileName]
        getIdentifier stack lineNum colNum (x : xs) tokens
          | isAlpha x || isDigit x || x == '_' = getIdentifier (stack ++ [x]) lineNum (colNum + 1) xs tokens
          | otherwise =
              tokenizeLine'
                lineNum
                (colNum + 1)
                []
                (x : xs)
                ( if stack == Keywords.moduleDecl
                    then tokens ++ [Token Module stack lineNum (colNum - length stack + 1) fileName]
                    else tokens ++ [Token Identifier stack lineNum (colNum - length stack + 1) fileName]
                )

        -- \| Extracts the next numeric from the given line.
        getNumeric :: [Char] -> Int -> Int -> String -> TokenList -> TokenList
        getNumeric stack lineNum colNum [] tokens = tokens ++ [Token NumericLiteral stack lineNum (colNum - length stack + 1) fileName]
        getNumeric stack lineNum colNum (x : xs) tokens
          | isDigit x = getNumeric (stack ++ [x]) lineNum (colNum + 1) xs tokens
          | x == '.' = getNumeric' (stack ++ [x]) lineNum (colNum + 1) xs tokens
          | otherwise = tokenizeLine' lineNum (colNum + 1) [] (x : xs) (tokens ++ [Token NumericLiteral stack lineNum (colNum - length stack + 1) fileName])
          where
            getNumeric' :: [Char] -> Int -> Int -> String -> TokenList -> TokenList
            getNumeric' stack lineNum colNum [] tokens = tokens ++ [Token NumericLiteral stack lineNum (colNum - length stack + 1) fileName]
            getNumeric' stack lineNum colNum (x : xs) tokens
              | isDigit x = getNumeric' (stack ++ [x]) lineNum (colNum + 1) xs tokens
              | otherwise = tokenizeLine' lineNum (colNum + 1) [] (x : xs) (tokens ++ [Token NumericLiteral stack lineNum (colNum - length stack + 1) fileName])