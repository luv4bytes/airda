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
import qualified LexerTypes
import qualified Operators

-- | Splits the given line into a list of tokens.
tokenizeLine :: String -> String -> Int -> LexerTypes.TokenList
tokenizeLine fileName line lineNum = tokenizeLine' lineNum 1 [] line []
  where
    tokenizeLine' :: Int -> Int -> [Char] -> String -> LexerTypes.TokenList -> LexerTypes.TokenList
    tokenizeLine' lineNum colNum stack [] tokens = tokens
    tokenizeLine' lineNum colNum stack (x : xs) tokens
      | isAlpha x =
          getIdentifier (stack ++ [x]) lineNum colNum xs tokens
      | isDigit x = getNumeric (stack ++ [x]) lineNum colNum xs tokens
      | x == '#' = tokenizeLine' lineNum (colNum + 1) stack [] tokens -- Comment. Whole line is not analyzed.
      | x == Operators.minus = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LexerTypes.Token LexerTypes.Minus [x] lineNum colNum fileName])
      | x == Operators.plus = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LexerTypes.Token LexerTypes.Plus [x] lineNum colNum fileName])
      | x == Operators.divide = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LexerTypes.Token LexerTypes.Divide [x] lineNum colNum fileName])
      | x == Operators.multiply = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LexerTypes.Token LexerTypes.Multiply [x] lineNum colNum fileName])
      | x == Operators.pow = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LexerTypes.Token LexerTypes.Power [x] lineNum colNum fileName])
      | x == Keywords.assignment =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LexerTypes.Token LexerTypes.Assignment [x] lineNum colNum fileName])
      | x == Keywords.typeSpecifier =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LexerTypes.Token LexerTypes.TypeSpecifier [x] lineNum (colNum - length [x] + 1) fileName])
      | x == Keywords.endOfStatement =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LexerTypes.Token LexerTypes.EndOfStatement [x] lineNum colNum fileName])
      | isSpace x = tokenizeLine' lineNum (colNum + 1) stack xs tokens
      | otherwise = tokenizeLine' lineNum (colNum + 1) stack xs (tokens ++ [LexerTypes.Token LexerTypes.Unknown [x] lineNum colNum fileName])
      where
        -- \| Extracts the next identifier from the given line.
        getIdentifier :: [Char] -> Int -> Int -> String -> LexerTypes.TokenList -> LexerTypes.TokenList
        getIdentifier stack lineNum colNum [] tokens
          | stack == Keywords.moduleDecl = tokens ++ [LexerTypes.Token LexerTypes.Module stack lineNum (colNum - length stack + 1) fileName]
          | otherwise = tokens ++ [LexerTypes.Token LexerTypes.Identifier stack lineNum (colNum - length stack + 1) fileName]
        getIdentifier stack lineNum colNum (x : xs) tokens
          | isAlpha x || isDigit x || x == '_' = getIdentifier (stack ++ [x]) lineNum (colNum + 1) xs tokens
          | otherwise =
              tokenizeLine'
                lineNum
                (colNum + 1)
                []
                (x : xs)
                ( if stack == Keywords.moduleDecl
                    then tokens ++ [LexerTypes.Token LexerTypes.Module stack lineNum (colNum - length stack + 1) fileName]
                    else tokens ++ [LexerTypes.Token LexerTypes.Identifier stack lineNum (colNum - length stack + 1) fileName]
                )

        -- \| Extracts the next numeric from the given line.
        getNumeric :: [Char] -> Int -> Int -> String -> LexerTypes.TokenList -> LexerTypes.TokenList
        getNumeric stack lineNum colNum [] tokens = tokens ++ [LexerTypes.Token LexerTypes.NumericLiteral stack lineNum (colNum - length stack + 1) fileName]
        getNumeric stack lineNum colNum (x : xs) tokens
          | isDigit x = getNumeric (stack ++ [x]) lineNum (colNum + 1) xs tokens
          | x == '.' = getNumeric' (stack ++ [x]) lineNum (colNum + 1) xs tokens
          | otherwise = tokenizeLine' lineNum (colNum + 1) [] (x : xs) (tokens ++ [LexerTypes.Token LexerTypes.NumericLiteral stack lineNum (colNum - length stack + 1) fileName])
          where
            getNumeric' :: [Char] -> Int -> Int -> String -> LexerTypes.TokenList -> LexerTypes.TokenList
            getNumeric' stack lineNum colNum [] tokens = tokens ++ [LexerTypes.Token LexerTypes.NumericLiteral stack lineNum (colNum - length stack + 1) fileName]
            getNumeric' stack lineNum colNum (x : xs) tokens
              | isDigit x = getNumeric' (stack ++ [x]) lineNum (colNum + 1) xs tokens
              | otherwise = tokenizeLine' lineNum (colNum + 1) [] (x : xs) (tokens ++ [LexerTypes.Token LexerTypes.NumericLiteral stack lineNum (colNum - length stack + 1) fileName])