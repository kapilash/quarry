#include "QToken.h"

namespace Quarry {
    Token::Token(int line, int column, enum TokenType tokenType) : line(line), column(column), tokenType(tokenType)
    {
    }

    Token::Token(const Token &other) : line(other.line), column(other.column), tokenType(other.tokenType)
    {
    }
    
}
