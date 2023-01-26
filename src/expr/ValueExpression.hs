module ValueExpression where

import qualified Error as ER
import qualified LexerTypes as LT
import qualified ParserTypes as PT

valueExpression :: PT.ParserState -> Either ER.ParserException (PT.TreeNode, PT.ParserState)
valueExpression [] = Left (ER.ParserExceptionSimple "Expected expression.")
valueExpression (t : ts)
  | LT.tokenType t == LT.NumericLiteral =
      Right
        ( PT.NumericLiteralNode (LT.tokenValue t),
          ts
        )
  | LT.tokenType t == LT.Identifier =
      Right
        ( PT.IdentifierNode (LT.tokenValue t),
          ts
        )
  | otherwise =
      Left
        ( ER.ParserException
            { ER.pexMessage = "Invalid expression '" ++ LT.tokenValue t ++ "'.",
              ER.pexErrCode = ER.errInvalidExpression,
              ER.pexLineNum = Just (LT.tokenLineNum t),
              ER.pexColNum = Just (LT.tokenColumn t),
              ER.pexFileName = LT.fileName t
            }
        )