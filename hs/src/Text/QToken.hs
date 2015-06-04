{-# Language BangPatterns #-}
module Text.QToken where


import qualified Data.Text as Txt

data QToken = QError Txt.Text
              | QKeyword !Int
              | QIdent !Txt.Text
              | QStrLiteral String
              | QCharLiteral Char
              | QDouble Double
              | QLong Integer Bool
              | QInt Int Bool
              | QFloat Float
              | QOperator Txt.Text
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
              | QComment Txt.Text
              | QBool
              | QWhitespace
              | QNewLine
              | QEOF
              | QMetaId !Txt.Text
              | QReferId !Int
            deriving (Show)

data QRecord = QRecord { qToken :: QToken,
                         qLine  :: Int,
                         qColumn :: Int
                       }
               deriving Show
