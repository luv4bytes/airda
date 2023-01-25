-- | Contains language keywords.
module Keywords where

-- | Defines the keyword for declaring modules.
moduleDecl :: String
moduleDecl = "Mod"

-- | Defines the keyword for the 'Num' (Numeric) type.
typeNumeric :: String
typeNumeric = "Num"

-- | Defines the keyword for the 'Int' (Integer) type.
typeInteger :: String
typeInteger = "Int"

-- | Defines the keyword for the 'Dec' (Decimal) type.
typeDecimal :: String
typeDecimal = "Dec"

-- | Assignment operator.
assignment :: Char
assignment = '='

-- | Type specifier.
typeSpecifier :: Char
typeSpecifier = ':'

-- | End of a statement.
endOfStatement :: Char
endOfStatement = ';'