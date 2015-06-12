{-# Language BangPatterns #-}
module Text.QToken where



data QToken = QError String
              | QKeyword !Int
              | QIdent String
              | QStrLiteral String
              | QCharLiteral Char
              | QDouble Double
              | QLong Integer Bool
              | QInt Int Bool
              | QFloat Float
              | QOperator !Int
              | QOpenBrace
              | QCloseBrace
              | QOpenBrackets
              | QCloseBrackets
              | QSquareOpen
              | QSquareClose
              | QDot
              | QComma
              | QQuestion
              | QColon
              | QSemiColon
              | QComment 
              | QBool
              | QWhitespace
              | QNewLine
              | QEOF
              | QMetaId String
              | QReferId !Int
            deriving (Show)

data QRecord = QRecord { qToken :: QToken,
                         qLine  :: Int,
                         qColumn :: Int
                       }
               deriving Show
