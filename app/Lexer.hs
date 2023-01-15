-- | Lexer contains functions to perform lexical analysis of sources.
module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Keywords as K
import qualified LexerTypes as LT

-- | Splits the given line into a list of tokens.
tokenizeLine :: String -> String -> Int -> LT.TokenList
tokenizeLine fileName line lineNum = tokenizeLine' lineNum 1 [] line []
  where
    tokenizeLine' :: Int -> Int -> [Char] -> String -> LT.TokenList -> LT.TokenList
    tokenizeLine' lineNum colNum stack [] tokens = tokens
    tokenizeLine' lineNum colNum stack (x : xs) tokens
      | isAlpha x =
          getIdentifier (stack ++ [x]) lineNum colNum xs tokens
      | isDigit x = getNumeric (stack ++ [x]) lineNum colNum xs tokens
      | x == '#' = tokenizeLine' lineNum (colNum + 1) stack [] tokens -- Comment. Whole line is not analyzed.
      | x == '-' = tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LT.Token LT.Minus [x] lineNum colNum fileName])
      | x == K.assignment =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LT.Token LT.Assignment [x] lineNum colNum fileName])
      | x == K.typeSpecifier =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LT.Token LT.TypeSpecifier [x] lineNum (colNum - length [x] + 1) fileName])
      | x == K.endOfStatement =
          tokenizeLine' lineNum (colNum + 1) [] xs (tokens ++ [LT.Token LT.EndOfStatement [x] lineNum colNum fileName])
      | isSpace x = tokenizeLine' lineNum (colNum + 1) stack xs tokens
      | otherwise = tokenizeLine' lineNum (colNum + 1) stack xs (tokens ++ [LT.Token LT.Unknown [x] lineNum colNum fileName])
      where
        -- \| Extracts the next identifier from the given line.
        getIdentifier :: [Char] -> Int -> Int -> String -> LT.TokenList -> LT.TokenList
        getIdentifier stack lineNum colNum [] tokens = tokens ++ [LT.Token LT.Identifier stack lineNum (colNum - length stack + 1) fileName]
        getIdentifier stack lineNum colNum (x : xs) tokens
          | isAlpha x || isDigit x || x == '_' = getIdentifier (stack ++ [x]) lineNum (colNum + 1) xs tokens
          | otherwise = tokenizeLine' lineNum (colNum + 1) [] (x : xs) (tokens ++ [LT.Token LT.Identifier stack lineNum (colNum - length stack + 1) fileName])

        -- \| Extracts the next numeric from the given line.
        getNumeric :: [Char] -> Int -> Int -> String -> LT.TokenList -> LT.TokenList
        getNumeric stack lineNum colNum [] tokens = tokens ++ [LT.Token LT.Numeric stack lineNum (colNum - length stack + 1) fileName]
        getNumeric stack lineNum colNum (x : xs) tokens
          | isDigit x = getNumeric (stack ++ [x]) lineNum (colNum + 1) xs tokens
          | x == '.' = getNumeric' (stack ++ [x]) lineNum (colNum + 1) xs tokens
          | otherwise = tokenizeLine' lineNum (colNum + 1) [] (x : xs) (tokens ++ [LT.Token LT.Numeric stack lineNum (colNum - length stack + 1) fileName])
          where
            getNumeric' :: [Char] -> Int -> Int -> String -> LT.TokenList -> LT.TokenList
            getNumeric' stack lineNum colNum [] tokens = tokens ++ [LT.Token LT.Numeric stack lineNum (colNum - length stack + 1) fileName]
            getNumeric' stack lineNum colNum (x : xs) tokens
              | isDigit x = getNumeric' (stack ++ [x]) lineNum (colNum + 1) xs tokens
              | otherwise = tokenizeLine' lineNum (colNum + 1) [] (x : xs) (tokens ++ [LT.Token LT.Numeric stack lineNum (colNum - length stack + 1) fileName])