{-# Language BangPatterns #-}
module Text.QToken where


data DoubleSuffix = SuffixF | SuffixD | SuffixM 

instance Show DoubleSuffix where
    show SuffixF = "F"
    show SuffixM = "M"
    show SuffixD = "D"

data IntegralSuffix = SuffixU
                      | SuffixL
                      | SuffixUL

instance Show IntegralSuffix where
    show SuffixU = "U"
    show SuffixL = "L"
    show SuffixUL = "UL"
    
data QToken = QError String
              | QKeyword !Int
              | QIdent String
              | QStrLiteral String
              | QCharLiteral Char
              | QDouble Double (Maybe DoubleSuffix)
              | QIntegral Integer (Maybe IntegralSuffix)
              | QOperator String
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
