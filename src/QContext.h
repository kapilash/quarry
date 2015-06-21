#pragma once

#include "QToken.h"
#include "QReader.h"

namespace Quarry {
    enum TokenStyle {
	CStyle,
	CSharpStyle,
	HaskellStyle,
	R6RSStyle,
	TeXStyle,
	LaTeXStyle,
	HTMLStyle
    };
    
    class QContext{
    private:
	const TokenStyle style;
	
    public:
	QContext (TokenStyle style);
	
	/// Idea is, given this tokenType, QContext knows how to construct
	/// the appropriate version of the token for the appropriate language.
	///  This works out for identifiers, keywords, comments, punctuation, etc.
	Token* makeToken(unsigned char *start, size_t length, TokenType tokenType);

	// When the lexer knows exactly the token to be constructed.
	Token* toToken(unsigned char *start, size_t length, int number);
    };
}
