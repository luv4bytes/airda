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

module Primitives where

-- | Defines the keyword for the primitive 'Num' (Numeric) type.
typeNumeric :: String
typeNumeric = "Num"

-- | Defines the keyword for the primitive 'Int' (Integer) type.
typeInteger :: String
typeInteger = "Int"

-- | Defines the keyword for the primitive 'Dec' (Decimal) type.
typeDecimal :: String
typeDecimal = "Dec"

-- | Defines the plus (+) operator.
plus :: Char
plus = '+'

-- | Defines the plus (+) operator as a string.
sPlus :: String
sPlus = "+"

-- | Defines the divide (/) operator.
divide :: Char
divide = '/'

-- | Defines the divide (/) operator as a string.
sDivide :: String
sDivide = "/"

-- | Defines the multiply (*) operator.
multiply :: Char
multiply = '*'

-- | Defines the multiply (*) operator as a string.
sMultiply :: String
sMultiply = "*"

-- | Defines the minus (-) operator.
minus :: Char
minus = '-'

-- | Defines the minus (-) operator as a string.
sMinus :: String
sMinus = "-"