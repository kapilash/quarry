/*
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2) Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of the Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
3) THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#pragma once

#include <map>
#include <string>
#include <set>
#include <vector>
#include "QReader.h"
#include "QToken.h"

namespace Quarry {

    class QContext ;

    typedef Token * (*Lexer)(QReader &reader, QContext &context);
    
    class QContext{
    public:
	QUARRY_EXPORT QContext(enum PL i);

	inline int keywordIndex(std::string &str) const {
	    auto it = keywords.find(str);
	    if (it == keywords.end()) {
		return -1;
	    }
	    return it->second;
	}
	
	inline int operatorIndex(std::string &str) const {
	    auto it = operators.find(str);
	    if (it == operators.end()) {
		return -1;
	    }
	    return it->second;
	}
	
	QUARRY_EXPORT inline Lexer lexer(unsigned char c) const {
	    return lexers[c];
	}

	QUARRY_EXPORT ~QContext();
    private:
	std::map<std::string, int> keywords;
	std::map<std::string, int> operators;
	std::vector<Lexer> lexers;
    };

    template<TokenType t>
	Token* singleCharLexer(QReader &reader, QContext &context) {
	int col = reader.getCol();
	int line = reader.getLine();
	const unsigned char *initPtr = reader.current();
	std::size_t pos = reader.currPosition();
	reader.next();
	return new Token(line, col, t, (reader.currPosition() - pos), initPtr );
    }

    QUARRY_EXPORT Token* spaceLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* charLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* cstringLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* numberLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* csComments(QReader &reader, QContext &context);

    QUARRY_EXPORT Token* csIdLexer(QReader &reader, QContext &context);

    QUARRY_EXPORT Token *csOperatorLexer(QReader &reader, QContext &context);
}
